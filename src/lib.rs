/*!
Extensible generational arena

```
use toy_arena::{Arena, Index};

#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
pub struct Entity {
    pub hp: usize,
}

let mut entities = Arena::<Entity>::with_capacity(1);
assert!(entities.len() == 0);
assert!(entities.capacity() == 1);
let index: Index<Entity> = entities.insert(Entity { hp: 0 });
```

# Similar crates
* [generational_arena](https://docs.rs/generational_arena/latest)
* [thunderdome](https://docs.rs/thunderdome/latest)
*/

use std::{fmt::Debug, hash::Hash, marker::PhantomData, ops};

use derivative::Derivative;
use nonmax::*;

/**
Generational arena

It's basically a [`Vec`], but with fixed item positions; arena operations don't move items. And
more, each item in the arena is given "generation" value, which identifies if the value has been
replaced by another value or id it's still there. Generation can be created [`PerSlot`] or
[`PerArena`].
*/
#[derive(Derivative)]
#[derivative(
    Default(bound = "<G as Gen>::PerArena: Default"),
    Debug(bound = "T: Debug, <G as Gen>::PerArena: Debug, <G as Gen>::PerSlot: Debug"),
    Clone(bound = "T: Clone, <G as Gen>::PerArena: Clone, <G as Gen>::PerSlot: Clone"),
    PartialEq(
        bound = "T: PartialEq, <G as Gen>::PerArena: PartialEq, <G as Gen>::PerSlot: PartialEq"
    ),
    Eq(bound = "T: PartialEq, <G as Gen>::PerArena: Eq, <G as Gen>::PerSlot: PartialEq"),
    Hash(bound = "T: Hash, <G as Gen>::PerArena: Hash, <G as Gen>::PerSlot: Hash")
)]
pub struct Arena<T, G: Gen = PerSlot> {
    data: Vec<Entry<T, G>>,
    /// Free slots
    free: Vec<Slot>,
    len: Slot,
    /// Per-arena generator. Zero-sized if we use [`PerArena`]
    gen: G::PerArena,
}

/* `dervative`:
We went to implement std traits only when all the fields implement that trait.
We can add `where field: Trait` bound for each field, but it exposes `Entry` type to the public
API, so we added indirect bounds above
*/

/// Index of an item in the belonging [`Arena`]
#[derive(Derivative)]
#[derivative(
    Debug(bound = " <G as Gen>::Generation: Debug"),
    Clone(bound = " <G as Gen>::Generation: Clone"),
    PartialEq(bound = "<G as Gen>::Generation: PartialEq"),
    Eq(bound = "<G as Gen>::Generation: PartialEq"),
    Hash(bound = "<G as Gen>::Generation: Hash")
)]
pub struct Index<T, G: Gen = PerSlot> {
    slot: Slot,
    gen: G::Generation,
    _ty: PhantomData<T>,
}

impl<T, G: Gen> Index<T, G> {
    fn new(slot: Slot, gen: G::Generation) -> Self {
        Self {
            slot,
            gen,
            _ty: PhantomData,
        }
    }
}

#[derive(Derivative)]
#[derivative(
    Default(bound = "<G as Gen>::PerSlot: Default"),
    Debug(bound = "Option<T>: Debug, <G as Gen>::PerSlot: Debug"),
    Clone(bound = "Option<T>: Clone, <G as Gen>::PerSlot: Clone"),
    PartialEq(bound = "Option<T>: PartialEq, <G as Gen>::PerSlot: PartialEq"),
    Eq(bound = "Option<T>: PartialEq, <G as Gen>::PerSlot: PartialEq"),
    Hash(bound = "Option<T>: Hash, <G as Gen>::PerSlot: Hash")
)]
struct Entry<T, G: Gen> {
    data: Option<T>,
    /// It's `()` if the generator is per-arena.
    gen: G::PerSlot,
}

type RawSlot = u32;

#[derive(Debug, Clone, Default, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[repr(transparent)]
pub struct Slot {
    raw: RawSlot,
}

impl Slot {
    fn inc(&mut self) {
        self.raw
            .checked_add(1)
            .unwrap_or_else(|| panic!("arena slot overflow"));
    }
}

/**
Generator of generations. Availables: per-slot or per-arena [`NonMaxU8`], [`NonMaxU16`],
[`NonMaxU32`] and [`NonMaxU32`],
*/
pub trait Gen {
    type Generation;
    type PerArena;
    type PerSlot;
    fn next(per_arena: &mut Self::PerArena, per_slot: &mut Self::PerSlot) -> Self::Generation;
}

/// Per-slot generation generator
#[derive(Derivative)]
#[derivative(Debug, Clone, Default, PartialEq, Eq, Hash)]
pub struct PerSlot<G = NonMaxU32> {
    _ty: PhantomData<G>,
}

/// Per-slot generation generator
#[derive(Derivative)]
#[derivative(Debug, Clone, Default, PartialEq, Eq, Hash)]
pub struct PerArena<G = NonMaxU32> {
    _ty: PhantomData<G>,
}

macro_rules! impl_generators {
    ($ty:ident) => {
        impl Gen for PerSlot<$ty> {
            type Generation = $ty;
            type PerArena = ();
            type PerSlot = Self::Generation;
            fn next(_per_arena: &mut Self::PerArena, per_slot: &mut Self::PerSlot) -> Self::Generation {
                let raw = per_slot.get();
                let new = $ty::new(raw + 1).expect("generation exceed");
                *per_slot = new;
                new
            }
        }

        impl Gen for PerArena<$ty> {
            type Generation = $ty;
            type PerArena = Self::Generation;
            type PerSlot = ();
            fn next(per_arena: &mut Self::PerArena, _per_slot: &mut Self::PerSlot) -> Self::Generation {
                let raw = per_arena.get();
                let new = $ty::new(raw + 1).expect("generation exceed");
                *per_arena = new;
                new
            }
        }
    };

    ($($ty:ident),+) => {
        $(
            impl_generators!($ty);
        )*
    };
}

impl_generators!(NonMaxU8, NonMaxU16, NonMaxU32, NonMaxU64);

impl<T, G: Gen> Arena<T, G> {
    pub fn len(&self) -> usize {
        self.len.raw as usize
    }

    pub fn capacity(&self) -> usize {
        self.data.capacity()
    }
}

impl<T, G: Gen> Arena<T, G> {
    pub fn new() -> Self
    where
        G::PerArena: Default,
    {
        Self::default()
    }

    pub fn with_capacity(cap: usize) -> Self
    where
        G::PerSlot: Default,
        G::PerArena: Default,
    {
        assert!(cap < RawSlot::MAX as usize, "Too big arena");

        let mut data = Vec::with_capacity(cap);
        for _ in 0..cap {
            data.push(Entry::default());
        }

        let free = (0u32..cap as RawSlot)
            .map(|x| Slot { raw: x })
            .collect::<Vec<_>>();

        Self {
            data,
            free,
            len: Slot::default(),
            gen: G::PerArena::default(),
        }
    }
}

impl<T, G: Gen> Arena<T, G>
where
    G::PerSlot: Default,
{
    fn next_slot(&mut self) -> Slot {
        if let Some(slot) = self.free.pop() {
            slot
        } else {
            self.extend(self.data.capacity() * 2);
            self.free.pop().unwrap()
        }
    }

    fn extend(&mut self, new_cap: usize) {
        assert!(self.data.capacity() > new_cap);
        assert!((new_cap as RawSlot) < RawSlot::MAX);
        let prev_cap = self.data.capacity();
        self.data.resize_with(new_cap, Default::default);

        self.free.resize_with(new_cap, Default::default);
        for raw in prev_cap..new_cap {
            self.free.push(Slot { raw: raw as u32 });
        }
    }

    pub fn insert(&mut self, data: T) -> Index<T, G> {
        let slot = self.next_slot();
        self.len.inc();
        let entry = &mut self.data[slot.raw as usize];
        assert!(entry.data.is_none());
        entry.data = Some(data);
        let gen = G::next(&mut self.gen, &mut entry.gen);
        Index::new(slot, gen)
    }
}
