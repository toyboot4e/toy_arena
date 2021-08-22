/*!
Extensible generational arena for various uses. [`Example`](_example)

Goals: Tiny code and real use. Non-goals: Super fast performance.

# Similar crates
* [generational_arena](https://docs.rs/generational_arena/latest)
* [thunderdome](https://docs.rs/thunderdome/latest)

NOTE: Still early, bugs can exist!
*/

// FIXME: don't use std::slice::IterMut

// use closures to implement `IntoIter`
#![feature(type_alias_impl_trait)]

pub mod _example;
pub mod iter;
pub mod tree;

#[cfg(test)]
mod test;

use std::{
    cell::{RefCell, UnsafeCell},
    fmt::{self, Debug},
    hash::Hash,
    iter::*,
    marker::PhantomData,
    mem,
    num::*,
    ops::{self, DerefMut},
};

use derivative::Derivative;
use smallvec::SmallVec;

use crate::iter::*;

/// Default generation type used by arena
pub type DefaultGen = NonZeroU32;

/**
Generational arena: basically a [`Vec`], but with fixed item positions

Arena operations don't move items. And more, each item in the arena is given "generation" value,
where we can distinguish new values from old values (and see if a value is already replaced by new
one).

[`Arena`] accepts three type parameters. `T`: item type, `D`: distinct type parameter for making
arena newtype, and `G` for the internal generation type. `D` and `G` have default types, so ordinary
`Arena` can be written as `Arena<T>`.

See also: [`crate::iter`].
*/
#[derive(Derivative)]
#[derivative(
    Debug(bound = "T: Debug"),
    Clone(bound = "T: Clone"),
    PartialEq(bound = "T: PartialEq"),
    Eq(bound = "T: Eq"),
    Hash(bound = "T: Hash")
)]
pub struct Arena<T, D = (), G: Gen = DefaultGen> {
    entries: Vec<Entry<T, G>>,
    /// Can be shared by the arena and a mutable iterator
    #[derivative(
        Hash(hash_with = "self::hash_unsafe_cell"),
        PartialEq(compare_with = "self::cmp_unsafe_cell"),
        Clone(clone_with = "self::clone_unsafe_cell")
    )]
    slot_states: UnsafeCell<SlotStates>,
    /// Distinct type parameter
    #[derivative(Debug = "ignore", PartialEq = "ignore", Hash = "ignore")]
    _distinct: PhantomData<fn() -> D>,
    // _distinct: PhantomData<*const D>,
}

fn hash_unsafe_cell<T: Hash, H: std::hash::Hasher>(x: &UnsafeCell<T>, state: &mut H) {
    unsafe {
        (*x.get()).hash(state);
    }
}

fn clone_unsafe_cell<T: Clone>(x: &UnsafeCell<T>) -> UnsafeCell<T> {
    unsafe { UnsafeCell::new((*x.get()).clone()) }
}

fn cmp_unsafe_cell<T: PartialEq>(x: &UnsafeCell<T>, other: &UnsafeCell<T>) -> bool {
    // compare address
    unsafe { &*x.get() == &*other.get() }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct SlotStates {
    n_items: Slot,
    free: Vec<Slot>,
}

/* Note on `dervative` use:
We went to implement std traits only when all the fields implement that trait.
We can add `where field: Trait` bound for each field, but it exposes `Entry` type to the public
API, so we added indirect bounds above
*/

#[derive(Derivative)]
#[derivative(
    Debug(bound = "T: Debug"),
    Clone(bound = "T: Clone"),
    PartialEq(bound = "T: PartialEq"),
    Eq(bound = "T: Eq"),
    Hash(bound = "T: Hash")
)]
struct Entry<T, G: Gen = DefaultGen> {
    gen: G,
    data: Option<T>,
}

/**
[`Arena::cell`] → virtual `Arena<RefCell<T>>` mode, but not tracking drop

# Example
```
use toy_arena::{Arena, Index};

let mut data = Arena::<usize>::new();
let v0 = data.insert(0);
let v1 = data.insert(1);

// arena cell, virtual `Arena<RefCell<T>>` mode
let cell = data.cell();

// first mutable borrow:
let v0_mut = cell.get_mut(v0).unwrap();
*v0_mut = 10;

// second borrow of the same item will cause panic at runtime:
// let v0_ref = cell.get(v0).unwrap();

// second mutable borrow of another item is allowed:
let v1_mut = cell.get_mut(v1).unwrap();
*v1_mut = 11;

drop(cell);

assert_eq!(data[v0], 10);
assert_eq!(data[v1], 11);
```
*/
#[derive(Derivative)]
#[derivative(Debug(bound = "T: Debug"))]
pub struct ArenaCell<'a, T, D = (), G: Gen = DefaultGen> {
    original: &'a mut Arena<T, D, G>,
    /// Use `UnsafeCell` for preformance and avoiding lifetime error when getting references to the
    /// items in it
    /// # Safety
    /// All the methods of [`ArenaCell`] are one-shot and we'll neve get two references to the
    /// internal arena at runtime.
    arena: UnsafeCell<Arena<T, D, G>>,
    log: RefCell<SmallVec<[(Slot, Borrow); 2]>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Borrow {
    Mutable,
    Immutable,
}

/**
[`Slot`] + [`Gen`]. Takes 8 bytes for [`DefaultGen`]

Item in the [`Arena`] is located by [`Slot`] and identified by their generation. If the item at a
slot is already replaced by another value, the generation of the entry is already incremented, so we
can identify the original item from the old, replaced item.
*/
#[derive(Derivative)]
#[derivative(Copy, Debug, Clone, PartialEq, Eq, Hash)]
pub struct Index<T, D = (), G: Gen = DefaultGen> {
    slot: Slot,
    gen: G,
    _ty: PhantomData<fn() -> T>,
    _distinct: PhantomData<fn() -> D>,
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

    pub fn gen(&self) -> G {
        self.gen
    }
}

type RawSlot = u32;

/**
Index of the backing `Vec` in [`Arena`]. Prefer mutable iterators to slot-based iteration

[`Arena::upgrade`] takes slot and returns index.
*/
#[derive(Copy, Debug, Clone, Default, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[repr(transparent)]
pub struct Slot {
    raw: RawSlot,
}

impl Into<usize> for Slot {
    fn into(self) -> usize {
        self.raw as usize
    }
}

impl fmt::Display for Slot {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self.raw, f)
    }
}

impl Slot {
    pub const ZERO: Self = Self { raw: 0 };

    /// Creates slot from raw value
    pub fn from_raw(raw: RawSlot) -> Self {
        Self { raw }
    }

    pub fn raw(&self) -> RawSlot {
        self.raw
    }

    /// NOTE: Slot is also used to track arena length
    fn inc_mut(&mut self) {
        self.raw = self
            .raw
            .checked_add(1)
            .unwrap_or_else(|| panic!("arena slot overflow"));
    }

    /// NOTE: Slot is also used to track arena length
    fn dec_mut(&mut self) {
        self.raw = self
            .raw
            .checked_sub(1)
            .unwrap_or_else(|| panic!("arena slot overflow"));
    }
}

/**
Generation type, one of the unsized `NonZero` types in [`std::num`]

Generation of a first item of a slot is always `1` (since it's using `NonZero` type and we'll
always increase the generation on creating new value).
*/
pub trait Gen: Debug + Clone + Copy + PartialEq + Eq + Hash + 'static {
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
        debug_assert!(cap < RawSlot::MAX as usize, "Too big arena");

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
            slot_states: UnsafeCell::new(SlotStates {
                free,
                n_items: Default::default(),
            }),
            _distinct: PhantomData,
        }
    }

    fn default_entry() -> Entry<T, G> {
        Entry {
            data: None,
            gen: G::default_gen(),
        }
    }

    /// Number of items in this arena
    pub fn len(&self) -> usize {
        let slot_states = unsafe { &*self.slot_states.get() };
        slot_states.n_items.raw as usize
    }

    /// Capacity of the backing vec
    pub fn capacity(&self) -> usize {
        self.entries.capacity()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

/// # ----- Mutations -----
impl<T, D, G: Gen> Arena<T, D, G> {
    pub fn insert(&mut self, data: T) -> Index<T, D, G> {
        let slot = self.next_free_slot();
        let entry = &mut self.entries[slot.raw as usize];

        let gen = {
            debug_assert!(entry.data.is_none(), "free slot occupied?");
            entry.data = Some(data);
            let slot_states = self.slot_states.get_mut();
            slot_states.n_items.inc_mut();
            entry.gen.next()
        };

        Index::<T, D, G>::new(slot, gen)
    }

    /// Removes all the items.
    pub fn clear(&mut self) {
        let mut slot_states = self.slot_states.get_mut();
        slot_states.free.clear();
        slot_states.n_items = Slot { raw: 0 };
    }

    /// Removes all the items and resets generation of every entry.
    pub fn clear_and_init_generations(&mut self) {
        self.clear();
        for e in &mut self.entries {
            e.gen = G::default_gen();
        }
    }

    /// Returns some item if the generation matchesA. Returns none on mismatch or no data
    pub fn remove(&mut self, index: Index<T, D, G>) -> Option<T> {
        let entry = &mut self.entries[index.slot.raw as usize];
        if entry.gen != index.gen || entry.data.is_none() {
            // generation mistmatch: can't remove
            None
        } else {
            Some(self::remove_binded(
                entry,
                self.slot_states.get_mut().deref_mut(),
                index,
            ))
        }
    }

    pub(crate) fn remove_by_slot(&mut self, slot: Slot) -> Option<T> {
        let index = self.upgrade(slot)?;
        let entry = &mut self.entries[index.slot.raw as usize];
        Some(self::remove_binded(
            entry,
            self.slot_states.get_mut().deref_mut(),
            index,
        ))
    }

    /// Returns none if the generation matches. Returns some index on mismatch or no data
    pub fn invalidate(&mut self, index: Index<T, D, G>) -> Option<Index<T, D, G>> {
        let entry = &mut self.entries[index.slot.raw as usize];
        self::invalidate(entry, self.slot_states.get_mut().deref_mut(), index)
    }

    pub fn replace(&mut self, index: Index<T, D, G>, new_data: T) -> Index<T, D, G> {
        if !(self.contains(index)) {
            self.insert(new_data)
        } else {
            let entry = &mut self.entries[index.slot.raw as usize];
            self::replace_binded(entry, index.slot, new_data)
        }
    }

    fn next_free_slot(&mut self) -> Slot {
        let slot_states = self.slot_states.get_mut();
        if let Some(slot) = slot_states.free.pop() {
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
            let mut cap = self.entries.capacity();
            if self.entries.capacity() == 0 {
                cap = 4;
            } else {
                cap *= 2;
            }
            drop(slot_states);
            self.extend(cap);
            self.next_free_slot()
        }
    }

    /// NOTE: After extending, len < capacity
    fn extend(&mut self, new_cap: usize) {
        debug_assert!(self.entries.capacity() < new_cap);
        debug_assert!((new_cap as RawSlot) < RawSlot::MAX);

        let prev_cap = self.entries.len();
        self.entries.resize_with(new_cap, Self::default_entry);

        // push in reverse (since the free stack is LIFO)
        for raw in (prev_cap as RawSlot..new_cap as RawSlot).rev() {
            let slot_states = self.slot_states.get_mut();
            slot_states.free.push(Slot { raw });
        }
    }

    /// Removes all items that don't satisfy the predicate
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

/// Borrows arena partially
pub(crate) fn remove_binded<T, D, G: Gen>(
    entry: &mut Entry<T, G>,
    slot_states: &mut SlotStates,
    index: Index<T, D, G>,
) -> T {
    debug_assert!(entry.data.is_some());
    let taken = entry.data.take().unwrap();
    slot_states.n_items.dec_mut();
    slot_states.free.push(index.slot);
    taken
}

/// Borrows arena partially
pub(crate) fn invalidate<T, D, G: Gen>(
    entry: &mut Entry<T, G>,
    slot_states: &mut SlotStates,
    index: Index<T, D, G>,
) -> Option<Index<T, D, G>> {
    if entry.gen != index.gen || entry.data.is_none() {
        // generation mismatch: can't invalidate
        Some(Index::new(index.slot, entry.gen.clone()))
    } else {
        entry.data = None;
        slot_states.n_items.dec_mut();
        slot_states.free.push(index.slot);
        None
    }
}

/// Borrows arena partially
pub(crate) fn replace_binded<T, D, G: Gen>(
    entry: &mut Entry<T, G>,
    slot: Slot,
    new: T,
) -> Index<T, D, G> {
    debug_assert!(entry.data.is_some());
    entry.data = Some(new);
    entry.gen.next();
    Index::<T, D, G>::new(slot, entry.gen)
}

/// # ----- Accessors -----
impl<T, D, G: Gen> Arena<T, D, G> {
    pub fn contains(&self, index: Index<T, D, G>) -> bool {
        self.entries.get(index.slot.raw as usize).is_some()
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

    /// # Safety
    /// Panics if the two indices point the same slot.
    pub fn get2_mut(
        &mut self,
        ix1: Index<T, D, G>,
        ix2: Index<T, D, G>,
    ) -> Option<(&mut T, &mut T)> {
        assert_ne!(ix1.slot(), ix2.slot());
        let x1 = self.get_mut(ix1)? as *mut _;
        let x2 = self.get_mut(ix2)?;
        Some(unsafe { (&mut *(x1 as *const _ as *mut _), x2) })
    }

    /// # Safety
    /// Panics if any two indices point to the same slot.
    pub fn get3_mut(
        &mut self,
        ix1: Index<T, D, G>,
        ix2: Index<T, D, G>,
        ix3: Index<T, D, G>,
    ) -> Option<(&mut T, &mut T, &mut T)> {
        assert!(ix1.slot() != ix2.slot() && ix2.slot() != ix3.slot() && ix3.slot() != ix1.slot());
        let x1 = self.get_mut(ix1)? as *mut _;
        let x2 = self.get_mut(ix2)? as *mut _;
        let x3 = self.get_mut(ix3)?;
        Some(unsafe {
            (
                &mut *(x1 as *const _ as *mut _),
                &mut *(x2 as *const _ as *mut _),
                x3,
            )
        })
    }

    /// Upgrades slot to `Index`. Prefer [`Arena::bindings`] when possible
    pub fn upgrade(&self, slot: Slot) -> Option<Index<T, D, G>> {
        if slot.raw as usize >= self.entries.len() {
            return None;
        }

        self.entries.get(slot.raw as usize).and_then(|e| {
            if e.data.is_some() {
                Some(Index::new(slot, e.gen))
            } else {
                None
            }
        })
    }

    /// See [`ArenaCell`] doc
    pub fn cell(&mut self) -> ArenaCell<T, D, G> {
        ArenaCell::new(self)
    }

    /// Internal use only
    pub(crate) fn get_by_slot(&self, slot: Slot) -> Option<&T> {
        self.entries.get(slot.raw as usize)?.data.as_ref()
    }

    /// Internal use only
    pub(crate) fn get_mut_by_slot(&mut self, slot: Slot) -> Option<&mut T> {
        self.entries.get_mut(slot.raw as usize)?.data.as_mut()
    }

    /// Internal use only
    pub(crate) fn get2_mut_by_slot(&mut self, s1: Slot, s2: Slot) -> Option<(&mut T, &mut T)> {
        debug_assert_ne!(s1, s2);
        let x1 = self.get_mut_by_slot(s1)? as *mut _;
        let x2 = self.get_mut_by_slot(s2)?;
        Some(unsafe { (&mut *x1, x2) })
    }
}

/// # ----- Iterators -----
impl<T, D, G: Gen> Arena<T, D, G> {
    /// `(Index, &T)`
    pub fn iter(&self) -> IndexedItemIter<T, D, G> {
        IndexedItemIter {
            entries: self.entries.iter().enumerate(),
            n_items: unsafe { &*self.slot_states.get() }.n_items.raw as usize,
            n_visited: 0,
            _distinct: PhantomData,
        }
    }

    /// `(Index, &mut T)`
    pub fn iter_mut(&mut self) -> IndexedItemIterMut<T, D, G> {
        IndexedItemIterMut {
            entries: self.entries.iter_mut().enumerate(),
            n_items: self.slot_states.get_mut().n_items.raw as usize,
            n_visited: 0,
            _distinct: PhantomData,
        }
    }

    /// `&T`
    pub fn items(&self) -> ItemIter<T, G> {
        ItemIter {
            entries: self.entries.iter(),
            n_items: unsafe { &*self.slot_states.get() }.n_items.raw as usize,
            n_visited: 0,
        }
    }

    /// `&mut T`
    pub fn items_mut(&mut self) -> ItemIterMut<T, G> {
        ItemIterMut {
            entries: self.entries.iter_mut(),
            n_items: self.slot_states.get_mut().n_items.raw as usize,
            n_visited: 0,
        }
    }

    /// `T`. Removes all items on drop
    pub fn drain(&mut self) -> Drain<T, D, G> {
        Drain {
            arena: self,
            slot: Slot::default(),
        }
    }

    /// See [`EntryBindings`] and [`EntryBind`]
    pub fn bindings(&mut self) -> EntryBindings<T, D, G> {
        EntryBindings::new(self)
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
    type IntoIter = IndexedItemIter<'a, T, D, G>;
    type Item = <Self::IntoIter as Iterator>::Item;
    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl<'a, T, D, G: Gen> IntoIterator for &'a mut Arena<T, D, G> {
    type IntoIter = IndexedItemIterMut<'a, T, D, G>;
    type Item = <Self::IntoIter as Iterator>::Item;
    fn into_iter(self) -> Self::IntoIter {
        self.iter_mut()
    }
}

impl<T, D, G: Gen> FromIterator<T> for Arena<T, D, G> {
    fn from_iter<I>(iter: I) -> Self
    where
        I: IntoIterator<Item = T>,
    {
        let mut arena = Self::new();
        for data in iter {
            arena.insert(data);
        }
        arena
    }
}

/**
Creates an arena and inserts given values. `Arena<T>` type might have to be annotated.

# Example
```
use toy_arena::{arena, Arena};
let data: Arena<usize> = arena![0, 1, 2, 3, 4];
```
*/
#[macro_export]
macro_rules! arena {
    ($($data:expr),*) => {{
        let mut arena = Arena::new();
        $(
            arena.insert($data);
        )*
        arena
    }}
}

/// Give back the roginal arena
impl<'a, T, D, G: Gen> Drop for ArenaCell<'a, T, D, G> {
    fn drop(&mut self) {
        mem::swap(self.original, self.arena.get_mut());
    }
}

impl<'a, T, D, G: Gen> ArenaCell<'a, T, D, G> {
    fn new(original: &'a mut Arena<T, D, G>) -> Self {
        // take the ownership of the
        let mut arena = Arena::new();
        mem::swap(original, &mut arena);

        Self {
            original,
            arena: UnsafeCell::new(arena),
            log: Default::default(),
        }
    }

    fn state(&self, slot: Slot) -> Option<Borrow> {
        self.log
            .borrow()
            .iter()
            .find(|(s, _b)| *s == slot)
            .map(|(_s, b)| *b)
    }

    pub fn contains(&self, index: Index<T, D, G>) -> bool {
        assert!(self.state(index.slot) != Some(Borrow::Mutable));
        let arena = unsafe { &*self.arena.get() };
        arena.contains(index)
    }

    pub fn get(&self, index: Index<T, D, G>) -> Option<&T> {
        let slot = index.slot;

        match self.state(slot) {
            None => self.log.borrow_mut().push((slot, Borrow::Immutable)),
            Some(Borrow::Immutable) => {}
            Some(Borrow::Mutable) => {
                panic!("Already borrowed (mutably)");
            }
        }

        let arena = unsafe { &*self.arena.get() };
        arena.get(index)
    }

    pub fn get_mut(&self, index: Index<T, D, G>) -> Option<&mut T> {
        let slot = index.slot;

        match self.state(slot) {
            None => self.log.borrow_mut().push((slot, Borrow::Mutable)),
            Some(Borrow::Immutable) => {
                panic!("Already borrowed (immutably)");
            }
            Some(Borrow::Mutable) => {
                panic!("Already borrowed (mutably)");
            }
        }

        // `&mut T` -> `*mut T` -> `&mut T`
        let arena = unsafe { &mut *self.arena.get() };
        let ref_mut = arena.get_mut(index)?;
        Some(unsafe { &mut *(ref_mut as *mut _) })
    }
}

// /// Shifts entries and removes empty slots. This operation will confuse existing indices.
// pub unsafe fn remove_empty_slots<T, D, G: Gen>(arena: &mut Arena<T, D, G>) {
//     todo!()
// }
//
// /// Sets every entry's generation to the smallest value. This operation will confuse existing
// /// indices.
// pub unsafe fn init_generations<T, D, G: Gen>(arena: &mut Arena<T, D, G>) {
//     todo!()
// }
