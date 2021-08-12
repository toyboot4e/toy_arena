use crate::*;

pub struct Drain<'a, T, D, G: Gen> {
    pub(crate) arena: &'a mut Arena<T, D, G>,
    pub(crate) slot: Slot,
}

impl<'a, T, D, G: Gen> Iterator for Drain<'a, T, D, G> {
    type Item = T;
    fn next(&mut self) -> Option<T> {
        while (self.slot.raw as usize) < self.arena.entries.len() {
            let slot = self.slot.clone();
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

/// &T
pub struct Items<'a, T, D, G: Gen> {
    pub(crate) arena: &'a Arena<T, D, G>,
    pub(crate) slot: Slot,
    pub(crate) n_visited: usize,
}

impl<'a, T, D, G: Gen> Iterator for Items<'a, T, D, G> {
    type Item = &'a T;
    fn next(&mut self) -> Option<Self::Item> {
        while self.n_visited < self.arena.len() {
            let slot = self.slot;
            self.slot.inc();

            let x = self.arena.get_by_slot(slot);
            if x.is_some() {
                self.n_visited += 1;
                return x;
            }
        }

        None
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let n = self.arena.len() - self.n_visited;
        (n, Some(n))
    }
}

impl<'a, T, D, G: Gen> FusedIterator for Items<'a, T, D, G> {}
impl<'a, T, D, G: Gen> ExactSizeIterator for Items<'a, T, D, G> {}

/// &mut T
pub struct ItemsMut<'a, T, D, G: Gen> {
    pub(crate) arena: &'a mut Arena<T, D, G>,
    pub(crate) slot: Slot,
    pub(crate) n_visited: usize,
}

impl<'a, T, D, G: Gen> Iterator for ItemsMut<'a, T, D, G> {
    type Item = &'a mut T;
    fn next(&mut self) -> Option<Self::Item> {
        while self.n_visited < self.arena.len() {
            let slot = self.slot;
            self.slot.inc();

            let x = self
                .arena
                .get_by_slot(slot)
                // ha ha.. trust me
                .map(|ptr| unsafe { &mut *(ptr as *const _ as *mut _) });
            if x.is_some() {
                self.n_visited += 1;
                return x;
            }
        }

        None
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let n = self.arena.len() - self.n_visited;
        (n, Some(n))
    }
}

impl<'a, T, D, G: Gen> FusedIterator for ItemsMut<'a, T, D, G> {}
impl<'a, T, D, G: Gen> ExactSizeIterator for ItemsMut<'a, T, D, G> {}

/// (Index, &T)
pub struct IndexedItems<'a, T, D, G: Gen> {
    pub(crate) arena: &'a Arena<T, D, G>,
    pub(crate) slot: Slot,
    pub(crate) n_visited: usize,
}

impl<'a, T, D, G: Gen> Iterator for IndexedItems<'a, T, D, G> {
    type Item = (Index<T, D, G>, &'a T);
    fn next(&mut self) -> Option<Self::Item> {
        while self.n_visited < self.arena.len() {
            let slot = self.slot;
            self.slot.inc();

            let entry = &self.arena.entries[slot.raw as usize];
            if let Some(data) = &entry.data {
                let index = Index::new(slot, entry.gen.clone());
                self.n_visited += 1;
                return Some((index, data));
            }
        }

        None
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let n = self.arena.len() - self.n_visited;
        (n, Some(n))
    }
}

impl<'a, T, D, G: Gen> FusedIterator for IndexedItems<'a, T, D, G> {}
impl<'a, T, D, G: Gen> ExactSizeIterator for IndexedItems<'a, T, D, G> {}

pub struct IndexedItemsMut<'a, T, D, G: Gen> {
    pub(crate) arena: &'a mut Arena<T, D, G>,
    pub(crate) slot: Slot,
    pub(crate) n_visited: usize,
}

impl<'a, T, D, G: Gen> Iterator for IndexedItemsMut<'a, T, D, G> {
    type Item = (Index<T, D, G>, &'a mut T);
    fn next(&mut self) -> Option<Self::Item> {
        while self.n_visited < self.arena.len() {
            let slot = self.slot;
            self.slot.inc();

            let entry = &mut self.arena.entries[slot.raw as usize];
            if let Some(data) = &mut entry.data {
                let index = Index::new(slot, entry.gen.clone());
                self.n_visited += 1;
                // trust me ha ha.. or we could try head/tail tactics:
                // https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=919ad1d07189eeb723de812e11cf3fca
                return Some((index, unsafe { &mut *(data as *const _ as *mut _) }));
            }
        }

        None
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let n = self.arena.len() - self.n_visited;
        (n, Some(n))
    }
}

impl<'a, T, D, G: Gen> FusedIterator for IndexedItemsMut<'a, T, D, G> {}
impl<'a, T, D, G: Gen> ExactSizeIterator for IndexedItemsMut<'a, T, D, G> {}
