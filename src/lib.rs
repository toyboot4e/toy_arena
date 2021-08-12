/*!
Extensible generational arena for various uses

Goals: Tiny code and real use. Non-goals: Extream memory efficiency and super good performance.

NOTE: While arena requires strict safety, `toy_arena` is not so tested (yet).

# Features
* Distinct arena types (second type parameter of [`Arena<T, D, G>`])
* Custom generation generator (third type parameter of [`Arena<T, D, G>`])

# Similar crates
* [generational_arena](https://docs.rs/generational_arena/latest)
* [thunderdome](https://docs.rs/thunderdome/latest)
*/

use std::{fmt::Debug, hash::Hash, marker::PhantomData, num::*, ops};

use derivative::Derivative;

/// Default generation generator
pub type DefaultGen = PerSlot;

/**
Generational arena

It's basically a [`Vec`], but with fixed item positions; arena operations don't move items. And
more, each item in the arena is given "generation" value, where we can distinguish new values from
original values (and see if the original value is still there or alreadly replaced). Generation can
be created [`PerSlot`] or [`PerArena`].
*/
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
    /// If `free` is empty, `entries[0..entries.len()]` is occupied
    free: Vec<Slot>,
    len: Slot,
    /// Per-arena generator. Zero-sized if we use [`PerArena`]
    gen: G::PerArena,
    /// Distinct type parameter
    #[derivative(Debug = "ignore", PartialEq = "ignore", Hash = "ignore")]
    pub _distinct: PhantomData<D>,
}

// /**
// Mutable access to multiple items in [`Arena`] at the cost of run-time check
// */
// #[derive(Derivative)]
// #[derivative(
//     Debug(bound = "&'a mut Arena<T, D, G>: Debug"),
//     Clone(bound = "&'a mut Arena<T, D, G>: Clone"),
//     PartialEq(bound = "&'a mut Arena<T, D, G>: PartialEq"),
//     Eq(bound = "&'a mut Arena<T, D, G>: PartialEq"),
//     Hash(bound = "&'a mut Arena<T, D, G>: Hash")
// )]
// pub struct ArenaCell<'a, T, , D = (),G: Gen = DefaultGen> {
//     log: smallvec::SmallVec<[Borrow; 2]>,
//     arena: &'a mut Arena<T, D, G>,
// }
//
// #[derive(Debug, Clone, PartialEq, Eq, Hash)]
// enum Borrow {
//     Mutable(Slot),
//     Immutable(Slot),
// }

/* `dervative`:
We went to implement std traits only when all the fields implement that trait.
We can add `where field: Trait` bound for each field, but it exposes `Entry` type to the public
API, so we added indirect bounds above
*/

/**
[`Slot`] + [`Generation`](Gen::Generation). Takes 8 bytes by default

Item in the [`Arena`] is located by [`Slot`] and identified by their generation. If the item at a
slot is already replaced by another value, the generation of the [`Entry`] is already incremented,
so we can identify the original item from replaced item.
*/
#[derive(Derivative)]
pub struct Index<T, D = (), G: Gen = DefaultGen> {
    slot: Slot,
    gen: G::Generation,
    _ty: PhantomData<T>,
    _distinct: PhantomData<D>,
}

impl<T, D, G: Gen> Index<T, D, G> {
    fn new(slot: Slot, gen: G::Generation) -> Self {
        Self {
            slot,
            gen,
            _ty: PhantomData,
            _distinct: PhantomData,
        }
    }

    pub fn slot(&self) -> Slot {
        self.slot.clone()
    }

    pub fn gen(&self, arena: &Arena<T, D, G>) -> G::Generation {
        self.gen.clone()
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
    /// It's `()` if the generator is per-arena.
    gen: G::PerSlot,
    data: Option<T>,
}

type RawSlot = u32;

/// Memory location in [`Arena`]
#[derive(Debug, Clone, Default, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[repr(transparent)]
pub struct Slot {
    raw: RawSlot,
}

impl Slot {
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

/**
Generator of generations [`PerSlot`] or [`PerArena`], backed by an unsigned `NonZero` type
*/
pub trait Gen {
    /// Identifier of new/orignal value, backed by one of the unsigned `NonZero` types
    type Generation: Debug + Clone + PartialEq + Eq + Hash;
    /// Per-arena generation generator
    type PerArena: Debug + Clone + PartialEq + Eq + Hash;
    /// Per-slot generation generator
    type PerSlot: Debug + Clone + PartialEq + Eq + Hash;
    /// Alternative to [`Self::Generation::default`]
    fn default_per_arena() -> Self::PerArena;
    fn default_per_slot() -> Self::PerSlot;
    fn current(per_arena: &Self::PerArena, per_slot: &Self::PerSlot) -> Self::Generation;
    fn next(per_arena: &mut Self::PerArena, per_slot: &mut Self::PerSlot) -> Self::Generation;
}

/// Specifies per-slot generation generator backed by an unsigned `NonZero` type
#[derive(Derivative)]
#[derivative(Debug, Clone, Default, PartialEq, Eq, Hash)]
pub struct PerSlot<G = NonZeroU32> {
    _ty: PhantomData<G>,
}

/// Specifies per-arena generation generator backed by an unsigned `NonZero` type
#[derive(Derivative)]
#[derivative(Debug, Clone, Default, PartialEq, Eq, Hash)]
pub struct PerArena<G = NonZeroU32> {
    _ty: PhantomData<G>,
}

macro_rules! impl_generators {
    ($nonzero:ident) => {
        impl Gen for PerSlot<$nonzero> {
            type Generation = $nonzero;
            type PerArena = ();
            type PerSlot = Self::Generation;
            fn default_per_arena() -> Self::PerArena {
                ()
            }
            fn default_per_slot() -> Self::PerSlot {
                unsafe { $nonzero::new_unchecked(1) }
            }
            fn current(_per_arena: &Self::PerArena, per_slot: &Self::PerSlot) -> Self::Generation {
                per_slot.clone()
            }
            fn next(_per_arena: &mut Self::PerArena, per_slot: &mut Self::PerSlot) -> Self::Generation {
                // Always increment. Initial item is given raw generation "2"
                let raw = per_slot.get();
                let new = $nonzero::new(raw + 1).expect("generation overflow");
                *per_slot = new;
                new
            }
        }

        impl Gen for PerArena<$nonzero> {
            type Generation = $nonzero;
            type PerArena = Self::Generation;
            type PerSlot = ();
            fn default_per_arena() -> Self::PerArena {
                unsafe { $nonzero::new_unchecked(1) }
            }
            fn default_per_slot() -> Self::PerSlot {
                ()
            }
            fn current(per_arena: &Self::PerArena, _per_slot: &Self::PerSlot) -> Self::Generation {
                per_arena.clone()
            }
            fn next(per_arena: &mut Self::PerArena, _per_slot: &mut Self::PerSlot) -> Self::Generation {
                // Always increment. Initial item is given raw generation "2"
                let raw = per_arena.get();
                let new = $nonzero::new(raw + 1).expect("generation overflow");
                *per_arena = new;
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
            gen: G::default_per_arena(),
            _distinct: PhantomData,
        }
    }

    fn default_entry() -> Entry<T, G> {
        Entry {
            data: None,
            gen: G::default_per_slot(),
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
            G::next(&mut self.gen, &mut entry.gen)
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
        if G::current(&self.gen, &entry.gen) != index.gen || entry.data.is_none() {
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
        let gen = G::current(&self.gen, &entry.gen);
        if gen != index.gen || entry.data.is_none() {
            // generation mismatch: can't invalidate
            Some(Index::new(index.slot, gen))
        } else {
            entry.data = None;
            self.len.dec();
            self.free.push(index.slot);
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
        self.entries.get(index.slot.raw as usize).and_then(|e| {
            let gen = G::current(&self.gen, &e.gen);
            if gen == index.gen {
                e.data.as_ref()
            } else {
                None
            }
        })
    }

    pub fn get_mut(&mut self, index: Index<T, D, G>) -> Option<&mut T> {
        // NOTE: Rust closure is not (yet) smart enough to borrow only some fileds of struct
        let (data, gen) = (&mut self.entries, &self.gen);
        data.get_mut(index.slot.raw as usize).and_then(|e| {
            let gen = G::current(&gen, &e.gen);
            if gen == index.gen {
                e.data.as_mut()
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
}

impl<T, D, G: Gen> Arena<T, D, G> {
    pub fn iter(&self) -> impl Iterator<Item = &T> + '_ {
        self.entries.iter().flat_map(|e| e.data.as_ref())
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

#[cfg(test)]
mod test {
    use super::*;
    use std::mem;

    #[test]
    fn test_size() {
        // `Index` is 8 bytes long by default
        assert_eq!(mem::size_of::<Index<()>>(), mem::size_of::<u32>() * 2);

        // the nonzero type reduces the optional index size
        assert_eq!(
            mem::size_of::<Option<Index<()>>>(),
            mem::size_of::<Index<()>>()
        );

        // unfortunatelly, entry is a bit too long with occupied tag
        assert_eq!(mem::size_of::<Entry<u32>>(), 12);
    }

    #[test]
    fn test_capacity() {
        #[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
        pub struct Entity {
            pub hp: usize,
        }

        let mut entities = Arena::<Entity>::with_capacity(2);
        assert_eq!(entities.len(), 0);
        assert_eq!(entities.capacity(), 2);

        let index0: Index<Entity> = entities.insert(Entity { hp: 0 });
        assert_eq!(index0.slot(), unsafe { Slot::from_raw(0) });
        assert_eq!(entities.len(), 1);
        assert_eq!(entities.capacity(), 2);
        assert_eq!(index0.gen(&entities), unsafe {
            // first generation
            NonZeroU32::new_unchecked(2)
        });

        let index1: Index<Entity> = entities.insert(Entity { hp: 1 });
        assert_eq!(index1.slot(), unsafe { Slot::from_raw(1) });
        assert_eq!(entities.len(), 2);

        let removed_entity = entities.remove(index0);
        assert_eq!(entities.len(), 1);
        assert_eq!(removed_entity, Some(Entity { hp: 0 }));

        let index0: Index<Entity> = entities.insert(Entity { hp: 10 });
        assert_eq!(entities.len(), 2);
        assert_eq!(index0.slot(), unsafe { Slot::from_raw(0) });
        assert_eq!(index0.gen(&entities), unsafe {
            // second generation
            NonZeroU32::new_unchecked(3)
        });

        // extend
        let index2: Index<Entity> = entities.insert(Entity { hp: 2 });
        assert_eq!(index2.slot(), unsafe { Slot::from_raw(2) });
        assert_eq!(entities.len(), 3);
        assert_eq!(entities.capacity(), 4);

        let index3: Index<Entity> = entities.insert(Entity { hp: 3 });
        assert_eq!(index3.slot(), unsafe { Slot::from_raw(3) });
        assert_eq!(entities.len(), 4);
        assert_eq!(entities.capacity(), 4);

        // extend
        let index4: Index<Entity> = entities.insert(Entity { hp: 4 });
        assert_eq!(index4.slot(), unsafe { Slot::from_raw(4) });
        assert_eq!(entities.len(), 5);
        assert_eq!(entities.capacity(), 8);
    }
}
