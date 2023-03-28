//! Iterator types for the [`Arena`]
//!
//! [`EntryBindings`] is a special one, where we can `remove`, `invaliate` or `replace` the binded
//! entries.

use crate::*;

/// [`Arena::drain`] → Iterator of `T`. Removes all arena items on drop
pub struct Drain<'a, T, G: Gen> {
    pub(crate) arena: &'a mut Arena<T, G>,
    pub(crate) slot: Slot,
}

impl<'a, T, G: Gen> Iterator for Drain<'a, T, G> {
    type Item = T;
    fn next(&mut self) -> Option<T> {
        while (self.slot.raw as usize) < self.arena.entries.len() {
            let slot = self.slot;
            self.slot.inc_mut();

            let index = match self.arena.upgrade(slot) {
                Some(i) => i,
                None => continue,
            };
            let data = self.arena.remove(index);
            if data.is_some() {
                return data;
            }
        }

        None
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.arena.len(), Some(self.arena.len()))
    }
}

impl<'a, T, G: Gen> FusedIterator for Drain<'a, T, G> {}
impl<'a, T, G: Gen> ExactSizeIterator for Drain<'a, T, G> {}

impl<'a, T, G: Gen> Drop for Drain<'a, T, G> {
    // Continue iterating/dropping if there are any elements left.
    fn drop(&mut self) {
        self.for_each(drop);
    }
}

macro_rules! borrow {
    // &'a T or &'a mut T
    (ref 'a T) => {
        &'a T
    };
    (mut 'a T) => {
        &'a mut T
    };

    // &data or &mut data
    (ref $e:expr) => {
        &$e
    };
    (mut $e:expr) => {
        &mut $e
    };
}

macro_rules! impl_item_iter {
    ($name:ident, $borrow:ident) => {
        impl_item_iter!(
            $name,
            $borrow,
            |me: &mut $name<'a, T, G>| me.entries.next(),
            |me: &mut $name<'a, T, G>| me.entries.next_back()
        );
    };

    ($name:ident, $borrow:ident, $next:expr, $next_back:expr) => {
        impl<'a, T, G: Gen> Iterator for $name<'a, T, G> {
            type Item = borrow!($borrow 'a T);
            fn next(&mut self) -> Option<Self::Item> {
                impl_item_iter!(self, $borrow, $next)
            }

            fn size_hint(&self) -> (usize, Option<usize>) {
                let n = self.n_items - self.n_visited;
                (n, Some(n))
            }
        }

        impl<'a, T, G: Gen> DoubleEndedIterator for $name<'a, T, G> {
            fn next_back(&mut self) -> Option<Self::Item> {
                impl_item_iter!(self, $borrow, $next_back)
            }
        }

        impl<'a, T, G: Gen> FusedIterator for $name<'a, T, G> {}
        impl<'a, T, G: Gen> ExactSizeIterator for $name<'a, T, G> {}
    };

    ($me:expr, $borrow:ident, $next:expr) => {{
        while $me.n_visited < $me.n_items {
            let entry = ($next)($me)?;
            if let Some(data) = borrow!($borrow entry.data) {
                $me.n_visited += 1;
                return Some(data);
            }
        }

        None
    }};
}

/// [`Arena::items`] → Iterator of `&T`
pub struct ItemIter<'a, T, G: Gen> {
    pub(crate) entries: std::slice::Iter<'a, Entry<T, G>>,
    pub(crate) n_items: usize,
    pub(crate) n_visited: usize,
}

impl_item_iter!(ItemIter, ref);

/// [`Arena::items_mut`] → Iterator of `&mut T`
pub struct ItemIterMut<'a, T, G: Gen> {
    pub(crate) entries: std::slice::IterMut<'a, Entry<T, G>>,
    /// Number of items in the arena
    pub(crate) n_items: usize,
    /// Number of visited items in the arena
    pub(crate) n_visited: usize,
}

impl_item_iter!(ItemIterMut, mut);

macro_rules! impl_indexed_iter {
    ($name:ident, $borrow:ident) => {
        impl_indexed_iter!(
            $name,
            $borrow,
            |entry: &mut $name<'a, T, G>| entry.next(),
            |entry: &mut $name<'a, T, G>| entry.next_back()
        );
    };

    ($name:ident, $borrow:ident, $next:expr, $next_back:expr) => {
        impl<'a, T, G: Gen> Iterator for $name<'a, T, G> {
            type Item = (Index<T, G>, borrow!($borrow 'a T));
            fn next(&mut self) -> Option<Self::Item> {
                impl_indexed_iter!(self, $borrow, $next)
            }

            fn size_hint(&self) -> (usize, Option<usize>) {
                let n = self.n_items - self.n_visited;
                (n, Some(n))
            }
        }

        impl<'a, T, G: Gen> DoubleEndedIterator for $name<'a, T, G> {
            fn next_back(&mut self) -> Option<Self::Item> {
                impl_indexed_iter!(self, $borrow, $next_back)
            }
        }

        impl<'a, T, G: Gen> FusedIterator for $name<'a, T, G> {}
        impl<'a, T, G: Gen> ExactSizeIterator for $name<'a, T, G> {}
    };

    ($me:expr, $borrow:ident, $next:expr) => {{
        while $me.n_visited < $me.n_items {
            let (slot, entry) = $me.entries.next()?;
            if let Some(data) = borrow!($borrow entry.data) {
                $me.n_visited += 1;
                let slot = Slot {
                    raw: slot as RawSlot,
                };
                let index = Index::new(slot, entry.gen.clone());
                return Some((index, data));
            }
        }

        None
    }};
}

/// [`Arena::iter`] → Iterator of `(Index, &T)`
pub struct IndexedItemIter<'a, T, G: Gen> {
    pub(crate) entries: Enumerate<std::slice::Iter<'a, Entry<T, G>>>,
    pub(crate) n_items: usize,
    pub(crate) n_visited: usize,
}

impl_indexed_iter!(IndexedItemIter, ref);

/// [`Arena::iter_mut`] → Iterator of `(Index, &mut T)`
pub struct IndexedItemIterMut<'a, T, G: Gen> {
    pub(crate) entries: Enumerate<std::slice::IterMut<'a, Entry<T, G>>>,
    pub(crate) n_items: usize,
    pub(crate) n_visited: usize,
}

impl_indexed_iter!(IndexedItemIterMut, mut);

macro_rules! impl_binds {
    ($name:ident) => {
        impl_binds!(
            $name,
            |me: &mut $name<'a, T, G>| me.entries.next(),
            |me: &mut $name<'a, T, G>| me.entries.next_back()
        );
    };

    ($name:ident, $next:expr, $next_back:expr) => {
        impl<'a, T, G: Gen> Iterator for $name<'a, T, G> {
            type Item = EntryBind<'a, T, G>;
            fn next(&mut self) -> Option<Self::Item> {
                impl_binds!(self, $next)
            }

            fn size_hint(&self) -> (usize, Option<usize>) {
                let n = self.n_items - self.n_visited;
                (n, Some(n))
            }
        }

        impl<'a, T, G: Gen> DoubleEndedIterator for $name<'a, T, G> {
            fn next_back(&mut self) -> Option<Self::Item> {
                impl_binds!(self, $next_back)
            }
        }

        impl<'a, T, G: Gen> FusedIterator for $name<'a, T, G> {}
        impl<'a, T, G: Gen> ExactSizeIterator for $name<'a, T, G> {}
    };

    ($me:expr, $next:expr) => {{
        while $me.n_visited < $me.n_items {
            let (slot, entry) = $next($me)?;
            if let Some(_data) = &mut entry.data {
                $me.n_visited += 1;
                let slot = Slot::from_raw(slot as RawSlot);
                let index = Index::new(slot, entry.gen.clone());
                return Some(EntryBind {
                    entry,
                    // NOTE: This is why we need unsafe cell
                    slot_states: $me.slot_states.get_mut(),
                    index,
                });
            }
        }

        None
    }};
}

/// [`Arena::bindings`] → Iterator of [`EntryBind`]
pub struct EntryBindings<'a, T, G: Gen> {
    entries: Enumerate<std::slice::IterMut<'a, Entry<T, G>>>,
    slot_states: &'a mut UnsafeCell<SlotStates>,
    //
    n_items: usize,
    n_visited: usize,
}

impl<'a, T, G: Gen> EntryBindings<'a, T, G> {
    pub(crate) fn new(arena: &'a mut Arena<T, G>) -> Self {
        // safety: no mutable alias
        let n_items = arena.slot_states.get_mut().n_items.into();
        Self {
            entries: arena.entries.iter_mut().enumerate(),
            slot_states: &mut arena.slot_states,
            //
            n_items,
            n_visited: 0,
        }
    }
}

impl_binds!(EntryBindings);

/// Mutable access to an arena entry
pub struct EntryBind<'a, T, G: Gen> {
    entry: &'a mut Entry<T, G>,
    slot_states: *mut SlotStates,
    index: Index<T, G>,
}

impl<'a, T, G: Gen> EntryBind<'a, T, G> {
    /// # Safety
    /// Panics if the data is removed/invaldated.
    pub fn get(&self) -> &T {
        self.entry.data.as_ref().unwrap()
    }

    /// # Safety
    /// Panics if the data is removed/invaldated.
    pub fn get_mut(&mut self) -> &mut T {
        self.entry.data.as_mut().unwrap()
    }

    pub fn index(&self) -> Index<T, G> {
        self.index
    }

    pub fn invalidate(self) -> Option<Index<T, G>> {
        crate::invalidate(self.entry, unsafe { &mut *self.slot_states }, self.index)
    }

    pub fn remove(self) -> T {
        crate::remove_binded(self.entry, unsafe { &mut *self.slot_states }, self.index)
    }

    pub fn replace(self, new: T) {
        crate::replace_binded::<T, G>(self.entry, self.index.slot, new);
    }
}
