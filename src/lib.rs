//! Generational [`Arena`] with type [`Index`].
//!
//! # Similar crates
//! * [generational_arena](https://docs.rs/generational_arena/latest)
//! * [thunderdome](https://docs.rs/thunderdome/latest)
//! * [pulz-arena](https://docs.rs/pulz-arena/latest)
//!
//! NOTE: Still early, bugs can exist!

// TODO: prefer fixed generations and slots

// use closures to implement `IntoIter`

// pub mod iter;
// pub mod tree;

#[cfg(test)]
mod test;

use std::{
    fmt::{self, Debug},
    hash::Hash,
    iter::*,
    marker::PhantomData,
    mem,
    num::*,
    ops,
};

#[cfg(feature = "igri")]
use igri::Inspect;

use derivative::Derivative;

// use crate::iter::*;

/// Default generation type used by arena.
pub type DefaultGen = NonZeroU32;

/// Generational arena with typed [`Index`].
///
/// It's basically a [`Vec`], but with fixed item positions.
#[derive(Derivative)]
#[derivative(
    Debug(bound = "T: Debug"),
    Clone(bound = "T: Clone"),
    PartialEq(bound = "T: PartialEq"),
    Eq(bound = "T: Eq"),
    Hash(bound = "T: Hash")
)]
#[cfg_attr(
    feature = "igri",
    derive(Inspect),
    inspect(with = "inspect_arena", bounds = "T: Inspect")
)]
pub struct Arena<T, G: Gen = DefaultGen> {
    entries: Vec<Entry<T, G>>,
    /// Linked list of free slots
    free: Option<Slot>,
    /// Number of filled entries
    len: usize,
}

#[cfg(feature = "igri")]
fn inspect_arena<'a, T, G: Gen>(arena: &'a mut Arena<T, G>, ui: &igri::imgui::Ui, label: &str)
where
    T: igri::Inspect,
{
    igri::seq_indexed(
        arena.entries.iter_mut().enumerate().filter_map(|(i, e)| {
            let data = e.data.as_mut()?;
            Some((i, data))
        }),
        ui,
        label,
    );
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
enum Entry<T, G: Gen = DefaultGen> {
    Filled { gen: G, data: T },
    Free { gen: G, next_free: Option<Slot> },
}

/// Slot with identitiy based on generation.
///
/// Item in the [`Arena`] is located by [`Slot`] and identified by their generation. If the item at
/// a slot is already replaced by another value, the generation of the entry is already incremented,
/// so we can identify the original item from the old, replaced item.
///
/// # Memory use
/// ```
/// use std::mem;
/// use toy_arena::Index;
/// assert_eq!(
///     mem::size_of::<Index<()>>(),
///     mem::size_of::<u64>(),
/// );
/// assert_eq!(
///     mem::size_of::<Option<Index<()>>>(),
///     mem::size_of::<u64>(),
/// );
/// ```
#[derive(Derivative)]
#[derivative(Copy, Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(
    feature = "igri",
    derive(Inspect),
    inspect(bounds = "", with = "inspect_index")
)]
pub struct Index<T, G: Gen = DefaultGen> {
    slot: Slot,
    gen: G,
    /// Item type parameter
    _t: PhantomData<fn() -> T>,
}

#[cfg(feature = "igri")]
fn inspect_index<'a, T, G: Gen>(index: &'a mut Index<T, G>, ui: &igri::imgui::Ui, label: &str) {
    index.slot.inspect(ui, label);
}

impl<T, G: Gen> Index<T, G> {
    fn new(slot: Slot, gen: G) -> Self {
        Self {
            slot,
            gen,
            _t: PhantomData,
        }
    }

    pub unsafe fn new_unsafe(raw_slot: RawSlot, gen: G) -> Self {
        Self::new(Slot::from_raw(raw_slot), gen)
    }

    pub fn slot(&self) -> Slot {
        self.slot
    }

    pub fn gen(&self) -> G {
        self.gen
    }
}

type RawSlot = u32;

/// Raw index of the backing `Vec` in [`Arena`]. It can be [upgraded](`Arena::upgrade`) to
/// [`Index`], but prefer mutable iterators for slot-based iteration.
#[derive(Copy, Debug, Clone, Default, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[cfg_attr(feature = "igri", derive(Inspect))]
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

    /// Creates slot from raw value.
    pub fn from_raw(raw: RawSlot) -> Self {
        Self { raw }
    }

    pub fn raw(&self) -> RawSlot {
        self.raw
    }

    pub const fn to_usize(&self) -> usize {
        self.raw as usize
    }

    /// NOTE: Slot is also used to track arena length.
    fn inc_mut(&mut self) {
        self.raw = self
            .raw
            .checked_add(1)
            .unwrap_or_else(|| panic!("arena slot overflow"));
    }

    /// NOTE: Slot is also used to track arena length.
    fn dec_mut(&mut self) {
        self.raw = self
            .raw
            .checked_sub(1)
            .unwrap_or_else(|| panic!("arena slot overflow"));
    }
}

/// Generation type, one of the unsized `NonZero` types in [`std::num`].
///
/// Generation of the first item at a slot is always `2` (since it's using `NonZero` type and we'll
/// always increase the generation on creating new value).
pub trait Gen: Debug + Clone + Copy + PartialEq + Eq + Hash + 'static {
    fn default_gen() -> Self;
    fn next(&self) -> Self;
}

macro_rules! impl_generators {
    ($nonzero:ident) => {
        impl Gen for $nonzero {
            fn default_gen() -> Self {
                unsafe { $nonzero::new_unchecked(1) }
            }
            fn next(&self) -> Self {
                // Always increment. Initial item is given raw generation "2"
                let raw = self.get();
                let new = $nonzero::new(raw + 1).expect("generation overflow");
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

impl<T, G: Gen> Default for Arena<T, G> {
    fn default() -> Self {
        Self::with_capacity(4)
    }
}

impl<T, G: Gen> Arena<T, G> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn with_capacity(cap: usize) -> Self {
        debug_assert!(cap < RawSlot::MAX as usize, "Too big arena");

        // fullfill the data with empty entries
        let mut data = Vec::with_capacity(cap);
        for i in 0..cap {
            data.push(Entry::Free {
                gen: G::default_gen(),
                next_free: if i != cap - 1 {
                    Some(Slot::from_raw(i as RawSlot + 1))
                } else {
                    None
                },
            });
        }

        Self {
            entries: data,
            free: if cap > 0 {
                Some(Slot::from_raw(0))
            } else {
                None
            },
            len: 0,
        }
    }

    /// Number of items in this arena.
    pub fn len(&self) -> usize {
        self.len
    }

    /// Capacity of the backing vec.
    pub fn capacity(&self) -> usize {
        self.entries.capacity()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

/// # ----- Mutations -----
impl<T, G: Gen> Arena<T, G> {
    /// Finds free slot and insert data.
    pub fn insert(&mut self, data: T) -> Index<T, G> {
        let slot = match self.free {
            Some(free_slot) => free_slot,
            None => {
                // no empty entry. alloc!
                self.entries.push(Entry::Filled {
                    gen: G::default_gen(),
                    data,
                });
                self.len += 1;

                return Index {
                    slot: Slot::from_raw(self.entries.len() as RawSlot),
                    gen: G::default_gen(),
                    _t: PhantomData,
                };
            }
        };

        let entry = &mut self.entries[slot.to_usize()];

        let gen = {
            let (gen, next_free) = match entry {
                Entry::Filled { .. } => {
                    panic!("bug: free slot occupied?");
                }
                Entry::Free { gen, next_free } => (*gen, *next_free),
            };

            let gen = gen.next();
            *entry = Entry::Filled { gen, data };

            self.free = next_free;
            self.len += 1;

            gen
        };

        Index::<T, G>::new(slot, gen)
    }

    /// Removes all the items, keeping the generation values.
    pub fn clear(&mut self) {
        let len = self.entries.len();

        // set up linked list of free slots
        for (i, entry) in self.entries.iter_mut().enumerate() {
            let gen = match entry {
                Entry::Filled { gen, .. } => gen.clone(),
                Entry::Free { .. } => continue,
            };

            *entry = Entry::Free {
                gen,
                // TODO: better isEnd (split last on iteration)
                next_free: if i != len - 1 {
                    None
                } else {
                    Some(Slot::from_raw(i as RawSlot + 1))
                },
            }
        }

        self.len = 0;

        self.free = if self.entries.is_empty() {
            None
        } else {
            Some(Slot::from_raw(0))
        };
    }

    /// Returns some item if the generation matchesA. Returns none on mismatch or no data.
    pub fn remove(&mut self, index: Index<T, G>) -> Option<T> {
        let entry = self.entries.get_mut(index.slot.to_usize())?;

        match entry {
            Entry::Filled { gen, .. } if *gen == index.gen => {
                let gen = *gen;
                let Entry::Filled { data, .. } = mem::replace(
                    entry,
                    Entry::Free {
                        gen: gen.next(),
                        next_free: self.free,
                    },
                ) else {
                    unreachable!("Arena::remove");
                };

                self.free = Some(index.slot());
                self.len -= 1;

                Some(data)
            }
            _ => None,
        }
    }

    pub(crate) fn remove_by_slot(&mut self, slot: Slot) -> Option<T> {
        let index = self.upgrade(slot)?;
        self.remove(index)
    }

    /// Faster [`remove`](Self::remove).
    pub fn invalidate(&mut self, index: Index<T, G>) -> Option<Index<T, G>> {
        let entry = self.entries.get_mut(index.slot.to_usize())?;

        match entry {
            Entry::Filled { gen, .. } if *gen == index.gen => {
                let gen = gen.next();
                Some(Index { gen, ..index })
            }
            _ => None,
        }
    }

    /// Replaces filled entry with given data or inserts it into a free slot
    pub fn replace(&mut self, index: Index<T, G>, new_data: T) -> Index<T, G> {
        if let Some(entry) = self.entries.get_mut(index.slot.to_usize()) {
            match entry {
                Entry::Filled { gen, data } => {
                    *gen = gen.next();
                    *data = new_data;

                    return Index {
                        gen: gen.next(),
                        ..index
                    };
                }
                _ => {}
            }
        }

        self.insert(new_data)
    }

    /// Removes all items that don't satisfy the predicate.
    pub fn retain<F: FnMut(Index<T, G>, &mut T) -> bool>(&mut self, mut pred: F) {
        let mut i = 0;
        while i < self.entries.len() {
            let entry = &mut self.entries[i];
            match entry {
                Entry::Filled { data, gen } => {
                    let slot = Slot { raw: i as RawSlot };
                    let index = Index::new(slot, *gen);
                    if !pred(index.clone(), data) {
                        self.remove(index).unwrap();
                    }
                }
                _ => {}
            }

            i += 1;
        }
    }

    /// Sets every entry's generation to the smallest value. This operation will confuse existing
    /// indices.
    pub unsafe fn reset_generations(&mut self) {
        for entry in &mut self.entries {
            match entry {
                Entry::Filled { gen, .. } => *gen = G::default_gen(),
                Entry::Free { gen, .. } => *gen = G::default_gen(),
            }
        }
    }
}

/// # ----- Accessors -----
impl<T, G: Gen> Arena<T, G> {
    pub fn contains(&self, index: Index<T, G>) -> bool {
        self.entries.get(index.slot.to_usize()).is_some()
    }

    pub fn get(&self, index: Index<T, G>) -> Option<&T> {
        self.entries
            .get(index.slot.to_usize())
            .and_then(|entry| match entry {
                Entry::Filled { gen, data } if *gen == index.gen => Some(data),
                _ => None,
            })
    }

    pub fn get_mut(&mut self, index: Index<T, G>) -> Option<&mut T> {
        // NOTE: Rust closure is not (yet) smart enough to borrow only some fileds of struct
        self.entries
            .get_mut(index.slot.to_usize())
            .and_then(|entry| match entry {
                Entry::Filled { gen, data } if *gen == index.gen => Some(data),
                _ => None,
            })
    }

    /// # Safety
    /// Panics if the two indices point the same slot.
    pub fn get2_mut(&mut self, ix1: Index<T, G>, ix2: Index<T, G>) -> Option<(&mut T, &mut T)> {
        assert_ne!(ix1.slot(), ix2.slot());
        let x1 = self.get_mut(ix1)? as *mut _;
        let x2 = self.get_mut(ix2)?;
        Some(unsafe { (&mut *(x1 as *const _ as *mut _), x2) })
    }

    /// # Safety
    /// Panics if any two indices point to the same slot.
    pub fn get3_mut(
        &mut self,
        ix1: Index<T, G>,
        ix2: Index<T, G>,
        ix3: Index<T, G>,
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

    /// Upgrades slot to `Index`. Prefer [`Arena::bindings`] when possible.
    pub fn upgrade(&self, slot: Slot) -> Option<Index<T, G>> {
        if slot.to_usize() >= self.entries.len() {
            return None;
        }

        self.entries.get(slot.to_usize()).and_then(|e| match e {
            Entry::Filled { gen, .. } => Some(Index::new(slot, *gen)),
            _ => None,
        })
    }

    /// Internal use only.
    pub(crate) fn get_by_slot(&self, slot: Slot) -> Option<&T> {
        match self.entries.get(slot.to_usize())? {
            Entry::Filled { data, .. } => Some(data),
            _ => None,
        }
    }

    /// Internal use only.
    pub(crate) fn get_mut_by_slot(&mut self, slot: Slot) -> Option<&mut T> {
        match self.entries.get_mut(slot.to_usize())? {
            Entry::Filled { data, .. } => Some(data),
            _ => None,
        }
    }

    /// Internal use only.
    pub(crate) fn get2_mut_by_slot(&mut self, s1: Slot, s2: Slot) -> Option<(&mut T, &mut T)> {
        debug_assert_ne!(s1, s2);
        let x1 = self.get_mut_by_slot(s1)? as *mut _;
        let x2 = self.get_mut_by_slot(s2)?;
        Some(unsafe { (&mut *x1, x2) })
    }
}

// /// # ----- Iterators -----
// impl<T, G: Gen> Arena<T, G> {
//     /// `(Index, &T)`
//     pub fn iter(&self) -> IndexedItemIter<T, G> {
//         IndexedItemIter {
//             entries: self.entries.iter().enumerate(),
//             n_items: unsafe { &*self.slot_states.get() }.n_items.to_usize(),
//             n_visited: 0,
//         }
//     }
//
//     /// `(Index, &mut T)`
//     pub fn iter_mut(&mut self) -> IndexedItemIterMut<T, G> {
//         IndexedItemIterMut {
//             entries: self.entries.iter_mut().enumerate(),
//             n_items: self.slot_states.get_mut().n_items.to_usize(),
//             n_visited: 0,
//         }
//     }
//
//     /// `&T`
//     pub fn items(&self) -> ItemIter<T, G> {
//         ItemIter {
//             entries: self.entries.iter(),
//             n_items: unsafe { &*self.slot_states.get() }.n_items.to_usize(),
//             n_visited: 0,
//         }
//     }
//
//     /// `&mut T`
//     pub fn items_mut(&mut self) -> ItemIterMut<T, G> {
//         ItemIterMut {
//             entries: self.entries.iter_mut(),
//             n_items: self.slot_states.get_mut().n_items.to_usize(),
//             n_visited: 0,
//         }
//     }
//
//     /// `T`. Removes all items on drop.
//     pub fn drain(&mut self) -> Drain<T, G> {
//         Drain {
//             arena: self,
//             slot: Slot::default(),
//         }
//     }
//
//     /// See [`EntryBindings`] and [`EntryBind`].
//     pub fn bindings(&mut self) -> EntryBindings<T, G> {
//         EntryBindings::new(self)
//     }
// }

impl<T, G: Gen> ops::Index<Index<T, G>> for Arena<T, G> {
    type Output = T;
    fn index(&self, index: Index<T, G>) -> &Self::Output {
        self.get(index).unwrap()
    }
}

impl<T, G: Gen> ops::IndexMut<Index<T, G>> for Arena<T, G> {
    fn index_mut(&mut self, index: Index<T, G>) -> &mut Self::Output {
        self.get_mut(index).unwrap()
    }
}

// impl<'a, T, G: Gen> IntoIterator for &'a Arena<T, G> {
//     type IntoIter = IndexedItemIter<'a, T, G>;
//     type Item = <Self::IntoIter as Iterator>::Item;
//     fn into_iter(self) -> Self::IntoIter {
//         self.iter()
//     }
// }
//
// impl<'a, T, G: Gen> IntoIterator for &'a mut Arena<T, G> {
//     type IntoIter = IndexedItemIterMut<'a, T, G>;
//     type Item = <Self::IntoIter as Iterator>::Item;
//     fn into_iter(self) -> Self::IntoIter {
//         self.iter_mut()
//     }
// }

impl<T, G: Gen> FromIterator<T> for Arena<T, G> {
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

/// Creates an [`Arena`] and with given values. [`Arena<T>`] type might have to be annotated.
///
/// # Example
/// ```
/// use toy_arena::{arena, Arena};
/// let data: Arena<usize> = arena![0, 1, 2, 3, 4];
/// ```
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
