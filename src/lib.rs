/*!
Extensible generational arena for various uses

Goals: Tiny code and real use. Non-goals: Extream performance and super safety.

NOTE: While arena requires strict safety, `toy_arena` is not so tested (yet).

# Features
* Distinct arena types (second type parameter of [`Arena<T, D, G>`])
* Customizable generation (third type parameter of [`Arena<T, D, G>`])

# Similar crates
* [generational_arena](https://docs.rs/generational_arena/latest)
* [thunderdome](https://docs.rs/thunderdome/latest)
*/

// use closures to implement `IntoIter`
#![feature(type_alias_impl_trait)]

pub mod iter;

#[cfg(test)]
mod test;

use std::{fmt::Debug, hash::Hash, iter::*, marker::PhantomData, num::*, ops};

use derivative::Derivative;
use smallvec::SmallVec;

use crate::iter::*;

/// Default generation type
pub type DefaultGen = NonZeroU32;

/**
Generational arena

It's basically a [`Vec`], but with fixed item positions; arena operations don't move items. And
more, each item in the arena is given "generation" value, where we can distinguish new values from
original values (and see if the original value is still there or alreadly replaced).*/
#[derive(Derivative)]
#[derivative(
    Debug(bound = "T: Debug"),
    Clone(bound = "T: Clone"),
    PartialEq(bound = "T: PartialEq"),
    Eq(bound = "T: PartialEq"),
    Hash(bound = "T: Hash")
)]
pub struct Arena<T, D = (), G: Gen = DefaultGen> {
    entries: Vec<Entry<T, G>>,
    /// Number of occupied entries
    len: Slot,
    /// If `free` is empty, `entries[0..entries.len()]` is occupied
    free: Vec<Slot>,
    /// Distinct type parameter
    #[derivative(Debug = "ignore", PartialEq = "ignore", Hash = "ignore")]
    pub _distinct: PhantomData<D>,
}

/**
Mutable access to multiple items in [`Arena`] at the cost of run-time check. Also the cell does NOT
track drops, so you can't borrow it anymore if you mutably borrow an item.
*/
#[derive(Derivative)]
#[derivative(
    Debug(bound = "&'a mut Arena<T, D, G>: Debug"),
    Clone(bound = "&'a mut Arena<T, D, G>: Clone"),
    PartialEq(bound = "&'a mut Arena<T, D, G>: PartialEq"),
    Eq(bound = "&'a mut Arena<T, D, G>: PartialEq"),
    Hash(bound = "&'a mut Arena<T, D, G>: Hash")
)]
pub struct ArenaCell<'a, T, D = (), G: Gen = DefaultGen> {
    arena: &'a mut Arena<T, D, G>,
    log: SmallVec<[(Slot, Borrow); 2]>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Borrow {
    Mutable,
    Immutable,
}

impl<'a, T, D, G: Gen> ArenaCell<'a, T, D, G> {
    fn new(arena: &'a mut Arena<T, D, G>) -> Self {
        Self {
            arena,
            log: Default::default(),
        }
    }

    fn state(&self, slot: Slot) -> Option<Borrow> {
        self.log.iter().find(|(s, _b)| *s == slot).map(|(_s, b)| *b)
    }

    pub fn contains(&self, index: Index<T, D, G>) -> bool {
        assert!(self.state(index.slot) != Some(Borrow::Mutable));
        self.arena.contains(index)
    }

    pub fn get(&self, index: Index<T, D, G>) -> Option<&T> {
        self.get_by_slot(index.slot)
    }

    pub fn get_mut(&self, index: Index<T, D, G>) -> Option<&mut T> {
        self.get_mut_by_slot(index.slot)
    }

    pub fn get_by_slot(&self, slot: Slot) -> Option<&T> {
        assert!(self.state(slot) != Some(Borrow::Mutable));
        self.arena.get_by_slot(slot)
    }

    pub fn get_mut_by_slot(&self, slot: Slot) -> Option<&mut T> {
        assert!(self.state(slot).is_none());
        let arena = unsafe { &mut *(self.arena as *const _ as *mut Arena<T, D, G>) };
        arena.get_mut_by_slot(slot)
    }
}

/* `dervative`:
We went to implement std traits only when all the fields implement that trait.
We can add `where field: Trait` bound for each field, but it exposes `Entry` type to the public
API, so we added indirect bounds above
*/

/**
[`Slot`] + [`Gen`]. Takes 8 bytes for [`DefaultGen`]

Item in the [`Arena`] is located by [`Slot`] and identified by their generation. If the item at a
slot is already replaced by another value, the generation of the entry is already incremented, so we
can identify the original item from replaced item.
*/
#[derive(Derivative)]
#[derivative(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Index<T, D = (), G: Gen = DefaultGen> {
    slot: Slot,
    gen: G,
    _ty: PhantomData<T>,
    _distinct: PhantomData<D>,
}

impl<T, D, G: Gen> Index<T, D, G> {
    fn new(slot: Slot, gen: G) -> Self {
        Self {
            slot,
            gen,
            _ty: PhantomData,
            _distinct: PhantomData,
        }
    }

    pub fn slot(&self) -> Slot {
        self.slot
    }
}

#[derive(Derivative)]
#[derivative(
    Debug(bound = "Option<T>: Debug"),
    Clone(bound = "Option<T>: Clone"),
    PartialEq(bound = "Option<T>: PartialEq"),
    Eq(bound = "Option<T>: PartialEq"),
    Hash(bound = "Option<T>: Hash")
)]
struct Entry<T, G: Gen = DefaultGen> {
    gen: G,
    data: Option<T>,
}

type RawSlot = u32;

/// Index of the backing `Vec` in [`Arena`]
#[derive(Copy, Debug, Clone, Default, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[repr(transparent)]
pub struct Slot {
    raw: RawSlot,
}

impl Slot {
    /// Creates slot from raw value
    /// # Safety
    /// if the raw slot > arena.len(), use of the slot will cause panic.
    pub unsafe fn from_raw(raw: RawSlot) -> Self {
        Self { raw }
    }

    pub fn to_raw(&self) -> RawSlot {
        self.raw
    }

    /// NOTE: Slot is also used to track arena length
    fn inc(&mut self) {
        self.raw = self
            .raw
            .checked_add(1)
            .unwrap_or_else(|| panic!("arena slot overflow"));
    }

    /// NOTE: Slot is also used to track arena length
    fn dec(&mut self) {
        self.raw = self
            .raw
            .checked_sub(1)
            .unwrap_or_else(|| panic!("arena slot overflow"));
    }
}

/// Generation type, one of the unsized `NonZero` types
pub trait Gen: Debug + Clone + PartialEq + Eq + Hash + 'static {
    fn default_gen() -> Self;
    fn next(&mut self) -> Self;
}

macro_rules! impl_generators {
    ($nonzero:ident) => {
        impl Gen for $nonzero {
            fn default_gen() -> Self {
                unsafe { $nonzero::new_unchecked(1) }
            }
            fn next(&mut self) -> Self {
                // Always increment. Initial item is given raw generation "2"
                let raw = self.get();
                let new = $nonzero::new(raw + 1).expect("generation overflow");
                *self = new;
                new
            }
        }
    };

    ($($nonzero:ident),+) => {
        $(
            impl_generators!($nonzero);
        )*
    };
}

impl_generators!(NonZeroU8, NonZeroU16, NonZeroU32, NonZeroU64);

impl<T, D, G: Gen> Arena<T, D, G> {
    /// Number of items in this arena
    pub fn len(&self) -> usize {
        self.len.raw as usize
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Capacity of the backing vec
    pub fn capacity(&self) -> usize {
        self.entries.capacity()
    }
}

impl<T, D, G: Gen> Default for Arena<T, D, G> {
    fn default() -> Self {
        Self::with_capacity(4)
    }
}

impl<T, D, G: Gen> Arena<T, D, G> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn with_capacity(cap: usize) -> Self {
        assert!(cap < RawSlot::MAX as usize, "Too big arena");

        // fullfill the data with empty entries
        let mut data = Vec::with_capacity(cap);
        for _ in 0..cap {
            data.push(Self::default_entry());
        }

        let free = (0..cap as RawSlot)
            .rev()
            .map(|raw| Slot { raw })
            .collect::<Vec<_>>();

        Self {
            entries: data,
            free,
            len: Slot::default(),
            _distinct: PhantomData,
        }
    }

    fn default_entry() -> Entry<T, G> {
        Entry {
            data: None,
            gen: G::default_gen(),
        }
    }
}

// State handling (length and free slots)
impl<T, D, G: Gen> Arena<T, D, G> {
    pub fn insert(&mut self, data: T) -> Index<T, D, G> {
        let slot = self.next_free_slot();
        let gen = {
            let entry = &mut self.entries[slot.raw as usize];
            assert!(entry.data.is_none(), "free slot occupied?");
            entry.data = Some(data);
            self.len.inc();
            entry.gen.next()
        };

        Index::<T, D, G>::new(slot, gen)
    }

    /// Removes all the items
    pub fn clear(&mut self) {
        self.free.clear();
        self.len = Slot { raw: 0 };
    }

    /// Returns some item if the generation matchesA. Returns none on mismatch or no data
    pub fn remove(&mut self, index: Index<T, D, G>) -> Option<T> {
        let entry = &mut self.entries[index.slot.raw as usize];
        if entry.gen != index.gen || entry.data.is_none() {
            // generation mistmatch: can't remove
            None
        } else {
            let taken = entry.data.take();
            assert!(taken.is_some());
            self.len.dec();
            self.free.push(index.slot);
            taken
        }
    }

    /// Returns none if the generation matches. Returns some index on mismatch or no data
    pub fn invalidate(&mut self, index: Index<T, D, G>) -> Option<Index<T, D, G>> {
        let entry = &mut self.entries[index.slot.raw as usize];
        if entry.gen != index.gen || entry.data.is_none() {
            // generation mismatch: can't invalidate
            Some(Index::new(index.slot, entry.gen.clone()))
        } else {
            entry.data = None;
            self.len.dec();
            self.free.push(index.slot);
            None
        }
    }

    pub fn remove_by_slot(&mut self, slot: Slot) -> Option<T> {
        let entry = &mut self.entries[slot.raw as usize];
        if entry.data.is_none() {
            // generation mistmatch: can't remove
            None
        } else {
            let taken = entry.data.take();
            assert!(taken.is_some());
            self.len.dec();
            self.free.push(slot);
            taken
        }
    }

    pub fn invalidate_by_slot(&mut self, slot: Slot) -> Option<Index<T, D, G>> {
        let entry = &mut self.entries[slot.raw as usize];
        if entry.data.is_none() {
            // generation mismatch: can't invalidate
            Some(Index::new(slot, entry.gen.clone()))
        } else {
            entry.data = None;
            self.len.dec();
            self.free.push(slot);
            None
        }
    }

    fn next_free_slot(&mut self) -> Slot {
        if let Some(slot) = self.free.pop() {
            slot
        } else if self.entries.len() < self.entries.capacity() {
            let slot = Slot {
                // NOTE: We know entries[0..entries.len()] is fullfiled
                // because `free` is fullfilled on init
                raw: self.entries.len() as RawSlot,
            };
            self.entries.push(Self::default_entry());
            slot
        } else {
            self.extend(self.entries.capacity() * 2);
            self.next_free_slot()
        }
    }

    /// NOTE: After extending, len < capacity
    fn extend(&mut self, new_cap: usize) {
        assert!(self.entries.capacity() < new_cap);
        assert!((new_cap as RawSlot) < RawSlot::MAX);

        let prev_cap = self.entries.len();
        self.entries.resize_with(new_cap, Self::default_entry);

        // push in reverse (since the free stack is LIFO)
        for raw in (prev_cap as RawSlot..new_cap as RawSlot).rev() {
            self.free.push(Slot { raw });
        }
    }
}

impl<T, D, G: Gen> Arena<T, D, G> {
    pub fn contains(&self, index: Index<T, D, G>) -> bool {
        self.get(index).is_some()
    }

    pub fn get(&self, index: Index<T, D, G>) -> Option<&T> {
        self.entries.get(index.slot.raw as usize).and_then(|entry| {
            if entry.gen == index.gen {
                entry.data.as_ref()
            } else {
                None
            }
        })
    }

    pub fn get_mut(&mut self, index: Index<T, D, G>) -> Option<&mut T> {
        // NOTE: Rust closure is not (yet) smart enough to borrow only some fileds of struct
        self.entries
            .get_mut(index.slot.raw as usize)
            .and_then(|entry| {
                if entry.gen == index.gen {
                    entry.data.as_mut()
                } else {
                    None
                }
            })
    }

    pub fn get_by_slot(&self, slot: Slot) -> Option<&T> {
        self.entries
            .get(slot.raw as usize)
            .and_then(|e| e.data.as_ref())
    }

    pub fn get_mut_by_slot(&mut self, slot: Slot) -> Option<&mut T> {
        self.entries
            .get_mut(slot.raw as usize)
            .and_then(|e| e.data.as_mut())
    }

    pub fn cell(&mut self) -> ArenaCell<T, D, G> {
        ArenaCell::new(self)
    }
}

impl<T, D, G: Gen> Arena<T, D, G> {
    /// (Index, &T)
    pub fn iter(&self) -> IndexedItems<T, D, G> {
        IndexedItems {
            arena: self,
            slot: Slot::default(),
            n_visited: 0,
        }
    }

    /// (Index, &mut T)
    pub fn iter_mut(&mut self) -> IndexedItemsMut<T, D, G> {
        IndexedItemsMut {
            arena: self,
            slot: Slot::default(),
            n_visited: 0,
        }
    }

    /// &T
    pub fn items(&self) -> Items<T, D, G> {
        Items {
            arena: self,
            slot: Slot::default(),
            n_visited: 0,
        }
    }

    /// &mut T
    pub fn items_mut(&mut self) -> ItemsMut<T, D, G> {
        ItemsMut {
            arena: self,
            slot: Slot::default(),
            n_visited: 0,
        }
    }

    pub fn drain(&mut self) -> Drain<T, D, G> {
        Drain {
            arena: self,
            slot: Slot::default(),
        }
    }

    pub fn retain<F: FnMut(Index<T, D, G>, &mut T) -> bool>(&mut self, mut pred: F) {
        let mut i = 0;
        while i < self.entries.len() {
            let entry = &mut self.entries[i];
            if let Some(data) = &mut entry.data {
                let slot = Slot { raw: i as RawSlot };
                let index = Index::new(slot, entry.gen.clone());
                if !pred(index.clone(), data) {
                    self.remove(index).unwrap();
                }
            }

            i += 1;
        }
    }
}

impl<T, D, G: Gen> ops::Index<Index<T, D, G>> for Arena<T, D, G> {
    type Output = T;
    fn index(&self, index: Index<T, D, G>) -> &Self::Output {
        self.get(index).unwrap()
    }
}

impl<T, D, G: Gen> ops::IndexMut<Index<T, D, G>> for Arena<T, D, G> {
    fn index_mut(&mut self, index: Index<T, D, G>) -> &mut Self::Output {
        self.get_mut(index).unwrap()
    }
}

impl<'a, T, D, G: Gen> IntoIterator for &'a Arena<T, D, G> {
    type IntoIter = IndexedItems<'a, T, D, G>;
    type Item = <Self::IntoIter as Iterator>::Item;
    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl<'a, T, D, G: Gen> IntoIterator for &'a mut Arena<T, D, G> {
    type IntoIter = IndexedItemsMut<'a, T, D, G>;
    type Item = <Self::IntoIter as Iterator>::Item;
    fn into_iter(self) -> Self::IntoIter {
        self.iter_mut()
    }
}
