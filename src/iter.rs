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

            let data = self.arena.remove_by_slot(slot);
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
pub struct Items<'a, T, G: Gen> {
    pub(crate) entries: std::slice::Iter<'a, Entry<T, G>>,
    pub(crate) n_items: usize,
    pub(crate) n_visited: usize,
}

impl<'a, T, G: Gen> Iterator for Items<'a, T, G> {
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

impl<'a, T, G: Gen> FusedIterator for Items<'a, T, G> {}
impl<'a, T, G: Gen> ExactSizeIterator for Items<'a, T, G> {}

/// [`Arena::items_mut`] → `&mut T`
pub struct ItemsMut<'a, T, G: Gen> {
    pub(crate) entries: std::slice::IterMut<'a, Entry<T, G>>,
    /// Number of items in the arena
    pub(crate) n_items: usize,
    /// Number of visited items in the arena
    pub(crate) n_visited: usize,
}

impl<'a, T, G: Gen> Iterator for ItemsMut<'a, T, G> {
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

impl<'a, T, G: Gen> FusedIterator for ItemsMut<'a, T, G> {}
impl<'a, T, G: Gen> ExactSizeIterator for ItemsMut<'a, T, G> {}

// /// [`Arena::iter`] → `(Index, &T)`
pub struct IndexedItems<'a, T, D, G: Gen> {
    pub(crate) entries: Enumerate<std::slice::Iter<'a, Entry<T, G>>>,
    pub(crate) n_items: usize,
    pub(crate) n_visited: usize,
    pub(crate) _distinct: PhantomData<D>,
}

impl<'a, T, D, G: Gen> Iterator for IndexedItems<'a, T, D, G> {
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

impl<'a, T, D, G: Gen> FusedIterator for IndexedItems<'a, T, D, G> {}
impl<'a, T, D, G: Gen> ExactSizeIterator for IndexedItems<'a, T, D, G> {}

/// [`Arena::iter_mut`] → `(Index, &mut T)`
pub struct IndexedItemsMut<'a, T, D, G: Gen> {
    pub(crate) entries: Enumerate<std::slice::IterMut<'a, Entry<T, G>>>,
    pub(crate) n_items: usize,
    pub(crate) n_visited: usize,
    pub(crate) _distinct: PhantomData<D>,
}

impl<'a, T, D, G: Gen> Iterator for IndexedItemsMut<'a, T, D, G> {
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

impl<'a, T, D, G: Gen> FusedIterator for IndexedItemsMut<'a, T, D, G> {}
impl<'a, T, D, G: Gen> ExactSizeIterator for IndexedItemsMut<'a, T, D, G> {}
