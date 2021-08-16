/*!
`toy_arena` iterator types
*/

use crate::*;

/// [`Arena::drain`] → `T`. Removes all items on drop
pub struct Drain<'a, T, D, G: Gen> {
    pub(crate) arena: &'a mut Arena<T, D, G>,
    pub(crate) slot: Slot,
}

impl<'a, T, D, G: Gen> Iterator for Drain<'a, T, D, G> {
    type Item = T;
    fn next(&mut self) -> Option<T> {
        while (self.slot.raw as usize) < self.arena.entries.len() {
            let slot = self.slot;
            self.slot.inc();

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

impl<'a, T, D, G: Gen> FusedIterator for Drain<'a, T, D, G> {}
impl<'a, T, D, G: Gen> ExactSizeIterator for Drain<'a, T, D, G> {}

impl<'a, T, D, G: Gen> Drop for Drain<'a, T, D, G> {
    // Continue iterating/dropping if there are any elements left.
    fn drop(&mut self) {
        self.for_each(drop);
    }
}

/// [`Arena::items`] → `&T`
pub struct ItemIter<'a, T, G: Gen> {
    pub(crate) entries: std::slice::Iter<'a, Entry<T, G>>,
    pub(crate) n_items: usize,
    pub(crate) n_visited: usize,
}

impl<'a, T, G: Gen> Iterator for ItemIter<'a, T, G> {
    type Item = &'a T;
    fn next(&mut self) -> Option<Self::Item> {
        while self.n_visited < self.n_items {
            let entry = self.entries.next()?;
            if let Some(data) = &entry.data {
                self.n_visited += 1;
                return Some(data);
            }
        }

        None
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let n = self.n_items - self.n_visited;
        (n, Some(n))
    }
}

impl<'a, T, G: Gen> FusedIterator for ItemIter<'a, T, G> {}
impl<'a, T, G: Gen> ExactSizeIterator for ItemIter<'a, T, G> {}

/// [`Arena::items_mut`] → `&mut T`
pub struct ItemIterMut<'a, T, G: Gen> {
    pub(crate) entries: std::slice::IterMut<'a, Entry<T, G>>,
    /// Number of items in the arena
    pub(crate) n_items: usize,
    /// Number of visited items in the arena
    pub(crate) n_visited: usize,
}

impl<'a, T, G: Gen> Iterator for ItemIterMut<'a, T, G> {
    type Item = &'a mut T;
    fn next(&mut self) -> Option<Self::Item> {
        while self.n_visited < self.n_items {
            let entry = self.entries.next()?;
            if let Some(data) = &mut entry.data {
                self.n_visited += 1;
                return Some(data);
            }
        }
        None
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let rest = self.n_items - self.n_visited;
        (rest, Some(rest))
    }
}

impl<'a, T, G: Gen> FusedIterator for ItemIterMut<'a, T, G> {}
impl<'a, T, G: Gen> ExactSizeIterator for ItemIterMut<'a, T, G> {}

/// [`Arena::iter`] → `(Index, &T)`
pub struct IndexedItemIter<'a, T, D, G: Gen> {
    pub(crate) entries: Enumerate<std::slice::Iter<'a, Entry<T, G>>>,
    pub(crate) n_items: usize,
    pub(crate) n_visited: usize,
    pub(crate) _distinct: PhantomData<fn() -> D>,
}

impl<'a, T, D, G: Gen> Iterator for IndexedItemIter<'a, T, D, G> {
    type Item = (Index<T, D, G>, &'a T);
    fn next(&mut self) -> Option<Self::Item> {
        while self.n_visited < self.n_items {
            let (slot, entry) = self.entries.next()?;
            if let Some(data) = &entry.data {
                self.n_visited += 1;
                let slot = Slot {
                    raw: slot as RawSlot,
                };
                let index = Index::new(slot, entry.gen.clone());
                return Some((index, data));
            }
        }

        None
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let n = self.n_items - self.n_visited;
        (n, Some(n))
    }
}

impl<'a, T, D, G: Gen> FusedIterator for IndexedItemIter<'a, T, D, G> {}
impl<'a, T, D, G: Gen> ExactSizeIterator for IndexedItemIter<'a, T, D, G> {}

/// [`Arena::iter_mut`] → `(Index, &mut T)`
pub struct IndexedItemIterMut<'a, T, D, G: Gen> {
    pub(crate) entries: Enumerate<std::slice::IterMut<'a, Entry<T, G>>>,
    pub(crate) n_items: usize,
    pub(crate) n_visited: usize,
    pub(crate) _distinct: PhantomData<fn() -> D>,
}

impl<'a, T, D, G: Gen> Iterator for IndexedItemIterMut<'a, T, D, G> {
    type Item = (Index<T, D, G>, &'a mut T);
    fn next(&mut self) -> Option<Self::Item> {
        while self.n_visited < self.n_items {
            let (slot, entry) = self.entries.next()?;
            if let Some(data) = &mut entry.data {
                self.n_visited += 1;
                let slot = Slot {
                    raw: slot as RawSlot,
                };
                let index = Index::new(slot, entry.gen.clone());
                return Some((index, data));
            }
        }

        None
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let rest = self.n_items - self.n_visited;
        (rest, Some(rest))
    }
}

impl<'a, T, D, G: Gen> FusedIterator for IndexedItemIterMut<'a, T, D, G> {}
impl<'a, T, D, G: Gen> ExactSizeIterator for IndexedItemIterMut<'a, T, D, G> {}

/// [`Arena::entries_mut`] → mutable access to arena entries
pub struct EntryBindings<'a, T, D, G: Gen> {
    pub(crate) arena: &'a mut Arena<T, D, G>,
    pub(crate) slot: Slot,
    pub(crate) n_items: usize,
    pub(crate) n_visited: usize,
}

impl<'a, T, D, G: Gen> EntryBindings<'a, T, D, G> {
    pub(crate) fn new(arena: &'a mut Arena<T, D, G>) -> Self {
        let n_items = arena.n_items.into();
        Self {
            arena,
            slot: Slot::default(),
            n_items,
            n_visited: 0,
        }
    }
}

impl<'a, T, D, G: Gen> Iterator for EntryBindings<'a, T, D, G> {
    type Item = EntryBind<'a, T, D, G>;
    fn next(&mut self) -> Option<Self::Item> {
        while self.n_visited < self.n_items {
            let slot = self.slot;
            self.slot.inc();

            let entry = &mut self.arena.entries[slot.raw as usize];
            if let Some(_data) = &mut entry.data {
                self.n_visited += 1;
                let index = Index::new(slot, entry.gen.clone());
                return Some(EntryBind {
                    // UNSAFE: cannot infer lifetime
                    arena: unsafe { &mut *(self.arena as *mut _) },
                    index,
                });
            }
        }

        None
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let rest = self.n_items - self.n_visited;
        (rest, Some(rest))
    }
}

/// Mutable access to an arena entry
pub struct EntryBind<'a, T, D, G: Gen> {
    arena: &'a mut Arena<T, D, G>,
    index: Index<T, D, G>,
}

impl<'a, T, D, G: Gen> EntryBind<'a, T, D, G>
where
    Index<T, D, G>: Copy,
{
    pub fn get(&self) -> &T {
        self.arena.get(self.index).unwrap()
    }

    pub fn get_mut(&mut self) -> &mut T {
        self.arena.get_mut(self.index).unwrap()
    }

    pub fn index(&self) -> Index<T, D, G> {
        self.index
    }

    pub fn invalidate(self) {
        let newer_node = self.arena.invalidate(self.index);
        debug_assert!(newer_node.is_none());
    }

    pub fn remove(self) -> T {
        self.arena.remove(self.index).unwrap()
    }

    pub fn replace(self, new: T) -> Self {
        let index = self.arena.replace(self.index, new);
        debug_assert_eq!(self.index.slot, index.slot);
        Self {
            arena: self.arena,
            index,
        }
    }
}
