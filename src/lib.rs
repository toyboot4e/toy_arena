/*!
Extensible generational arena

# Similar crates
* [generational_arena](https://docs.rs/generational_arena/latest)
* [thunderdome](https://docs.rs/thunderdome/latest)
*/

use std::{fmt::Debug, hash::Hash, marker::PhantomData, ops};

use derivative::Derivative;
use nonmax::*;

/**
Generational arena

[`Arena`] is basically a [`Vec`], but it fixes item positions. Each item in the arena is given
"generation" value, which identifies if the value has been replaced with other value or still there.
*/
// `dervative`:
// We went to implement std traits only when all the fields implement the trait.
// We can add `where field: Trait` bound for each field, but it exposes `Entry` type to the public
// API, so here we're adding indirect bounds:
#[derive(Derivative)]
#[derivative(
    Default(bound = "G: Default"),
    Debug(bound = "T: Debug, G: Debug, <G as Gen>::PerSlot: Debug"),
    Clone(bound = "T: Clone, G: Clone, <G as Gen>::PerSlot: Clone"),
    PartialEq(bound = "T: PartialEq, G: PartialEq, <G as Gen>::PerSlot: PartialEq"),
    Eq(bound = "T: PartialEq, G: Eq, <G as Gen>::PerSlot: PartialEq"),
    Hash(bound = "T: Hash, G: Hash, <G as Gen>::PerSlot: Hash")
)]
pub struct Arena<T, G: Gen = PerSlot> {
    data: Vec<Entry<T, G>>,
    gen: G,
}

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

#[derive(Derivative)]
#[derivative(
    Debug(bound = "Option<T>: Debug, <G as Gen>::PerSlot: Debug"),
    Clone(bound = "Option<T>: Clone, <G as Gen>::PerSlot: Clone"),
    PartialEq(bound = "Option<T>: PartialEq, <G as Gen>::PerSlot: PartialEq"),
    Eq(bound = "Option<T>: PartialEq, <G as Gen>::PerSlot: PartialEq"),
    Hash(bound = "Option<T>: Hash, <G as Gen>::PerSlot: Hash")
)]
struct Entry<T, G: Gen> {
    /// Item positions are fixed.
    data: Option<T>,
    /// It's `()` if the generator is per-arena.
    gen: G::PerSlot,
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[repr(transparent)]
pub struct Slot {
    raw: u32,
}

/// Generator of generations. Availables: per-slot or per-arena [`NonMaxU8`], [`NonMaxU16`],
/// [`NonMaxU32`] and [`NonMaxU32`],
pub trait Gen {
    type Generation;
    type PerSlot;
    fn next(per_arena: &mut Self, per_slot: &mut Self::PerSlot) -> Self::Generation;
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
    gen: G,
}

macro_rules! impl_generators {
    ($ty:ident) => {
        impl Gen for PerSlot<$ty> {
            type Generation = $ty;
            type PerSlot = $ty;
            fn next(_per_arena: &mut Self, per_slot: &mut Self::PerSlot) -> Self::Generation {
                let raw = per_slot.get();
                let new = $ty::new(raw + 1).expect("generation exceed");
                *per_slot = new;
                new
            }
        }

        impl Gen for PerArena<$ty> {
            type Generation = $ty;
            type PerSlot = ();
            fn next(per_arena: &mut Self, _per_slot: &mut Self::PerSlot) -> Self::Generation {
                let raw = per_arena.gen.get();
                let new = $ty::new(raw + 1).expect("generation exceed");
                per_arena.gen = new;
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

impl<T, G: Gen + Default> Arena<T, G> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn with_capacity(cap: usize) -> Self {
        assert!(
            cap < u32::MAX as usize,
            "Unable to create an `Arena` bigger than u32 length"
        );

        Self {
            data: Vec::with_capacity(cap),
            gen: G::default(),
        }
    }
}

impl<T, G: Gen> Arena<T, G> {
    pub fn len(&self) -> usize {
        self.data.len()
    }

    pub fn capacity(&self) -> usize {
        self.data.len()
    }
}
