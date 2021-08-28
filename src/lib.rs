/*!
Extensible generational arena for various uses. [`Example`](_example)

Goals: Tiny code and real use. Non-goals: Super fast performance.

# Similar crates
* [generational_arena](https://docs.rs/generational_arena/latest)
* [thunderdome](https://docs.rs/thunderdome/latest)

NOTE: Still early, bugs can exist!
*/

// TODO: prefer immutable generations and slots

// use closures to implement `IntoIter`

pub mod _example;
pub mod iter;
pub mod tree;

#[cfg(test)]
mod test;

use std::{
    cell::UnsafeCell,
    fmt::{self, Debug},
    hash::Hash,
    iter::*,
    marker::PhantomData,
    num::*,
    ops::{self, DerefMut},
};

use derivative::Derivative;

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
Slot with identitiy based on generation.

Item in the [`Arena`] is located by [`Slot`] and identified by their generation. If the item at a
slot is already replaced by another value, the generation of the entry is already incremented, so we
can identify the original item from the old, replaced item.

# Memory use
```
use std::mem;
use toy_arena::Index;
assert_eq!(
    mem::size_of::<Index<()>>(),
    mem::size_of::<u64>(),
);
assert_eq!(
    mem::size_of::<Option<Index<()>>>(),
    mem::size_of::<u64>(),
);
```
*/
#[derive(Derivative)]
#[derivative(Copy, Debug, Clone, PartialEq, Eq, Hash)]
pub struct Index<T, D = (), G: Gen = DefaultGen> {
    slot: Slot,
    gen: G,
    /// Item type parameter
    _t: PhantomData<fn() -> T>,
    /// Distinct type parameter
    _d: PhantomData<fn() -> D>,
}

/// # ---- Common impls -----
impl<T, D, G: Gen> Index<T, D, G> {
    fn new(slot: Slot, gen: G) -> Self {
        Self {
            slot,
            gen,
            _t: PhantomData,
            _d: PhantomData,
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
Index of the backing `Vec` in [`Arena`]. It can be [upgraded](`Arena::upgrade`) to [`Index`], but
prefer mutable iterators to slot-based iteration.

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

    pub const fn to_usize(&self) -> usize {
        self.raw as usize
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

Generation of the first item at a slot is always `2` (since it's using `NonZero` type and we'll
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
        slot_states.n_items.to_usize()
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
        let entry = &mut self.entries[slot.to_usize()];

        let gen = {
            debug_assert!(entry.data.is_none(), "bug: free slot occupied?");
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
        let entry = &mut self.entries[index.slot.to_usize()];
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
        let entry = &mut self.entries[index.slot.to_usize()];
        Some(self::remove_binded(
            entry,
            self.slot_states.get_mut().deref_mut(),
            index,
        ))
    }

    /// Returns none if the generation matches. Returns some index on mismatch or no data
    pub fn invalidate(&mut self, index: Index<T, D, G>) -> Option<Index<T, D, G>> {
        let entry = &mut self.entries[index.slot.to_usize()];
        self::invalidate(entry, self.slot_states.get_mut().deref_mut(), index)
    }

    /// Inalidates given index. Returns some index on updating the generation
    pub fn invalidate_indices(&mut self, index: Index<T, D, G>) -> Option<Index<T, D, G>> {
        let entry = &mut self.entries[index.slot.to_usize()];
        if index.gen == entry.gen && entry.data.is_some() {
            entry.gen = entry.gen.clone().next();
            Some(Index {
                gen: entry.gen,
                slot: index.slot,
                _t: PhantomData,
                _d: PhantomData,
            })
        } else {
            None
        }
    }

    pub fn replace(&mut self, index: Index<T, D, G>, new_data: T) -> Index<T, D, G> {
        if !(self.contains(index)) {
            self.insert(new_data)
        } else {
            let entry = &mut self.entries[index.slot.to_usize()];
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

    /// Sets every entry's generation to the smallest value. This operation will confuse existing
    /// indices.
    pub unsafe fn reset_generations(&mut self) {
        for entry in &mut self.entries {
            entry.gen = G::default_gen();
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
        self.entries.get(index.slot.to_usize()).is_some()
    }

    pub fn get(&self, index: Index<T, D, G>) -> Option<&T> {
        self.entries.get(index.slot.to_usize()).and_then(|entry| {
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
            .get_mut(index.slot.to_usize())
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
        if slot.to_usize() >= self.entries.len() {
            return None;
        }

        self.entries.get(slot.to_usize()).and_then(|e| {
            if e.data.is_some() {
                Some(Index::new(slot, e.gen))
            } else {
                None
            }
        })
    }

    /// Internal use only
    pub(crate) fn get_by_slot(&self, slot: Slot) -> Option<&T> {
        self.entries.get(slot.to_usize())?.data.as_ref()
    }

    /// Internal use only
    pub(crate) fn get_mut_by_slot(&mut self, slot: Slot) -> Option<&mut T> {
        self.entries.get_mut(slot.to_usize())?.data.as_mut()
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
            n_items: unsafe { &*self.slot_states.get() }.n_items.to_usize(),
            n_visited: 0,
            _distinct: PhantomData,
        }
    }

    /// `(Index, &mut T)`
    pub fn iter_mut(&mut self) -> IndexedItemIterMut<T, D, G> {
        IndexedItemIterMut {
            entries: self.entries.iter_mut().enumerate(),
            n_items: self.slot_states.get_mut().n_items.to_usize(),
            n_visited: 0,
            _distinct: PhantomData,
        }
    }

    /// `&T`
    pub fn items(&self) -> ItemIter<T, G> {
        ItemIter {
            entries: self.entries.iter(),
            n_items: unsafe { &*self.slot_states.get() }.n_items.to_usize(),
            n_visited: 0,
        }
    }

    /// `&mut T`
    pub fn items_mut(&mut self) -> ItemIterMut<T, G> {
        ItemIterMut {
            entries: self.entries.iter_mut(),
            n_items: self.slot_states.get_mut().n_items.to_usize(),
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
Creates an [`Arena`] and with given values. [`Arena<T>`] type might have to be annotated.

# Example
```
use toy_arena::{arena, Arena};
let data: Arena<usize> = arena![0, 1, 2, 3, 4];
```
*/
#[macro_export]
macro_rules! arena {
    ($($data:expr),*) => {{
        let mut arena = $crate::Arena::new();
        $(
            arena.insert($data);
        )*
        arena
    }}
}
