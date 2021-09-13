/*!
Link of tree node IDs
*/

use std::fmt;

pub trait Tree {
    /// usize or small size
    type Slot: Default + Clone + PartialEq + fmt::Debug;

    /// Slot with generation
    type Id: Id<Self::Slot> + Clone + fmt::Debug;

    fn root_mut(&mut self) -> &mut Link<Self::Slot>;

    fn link_mut_by_slot(&mut self, slot: Self::Slot) -> Option<&mut Link<Self::Slot>>;

    fn link2_mut_by_slot(
        &mut self,
        s0: Self::Slot,
        s1: Self::Slot,
    ) -> Option<(&mut Link<Self::Slot>, &mut Link<Self::Slot>)>;

    fn link_mut_by_id(&mut self, id: Self::Id) -> Option<&mut Link<Self::Slot>>;
}

pub trait Id<S> {
    fn slot(&self) -> S;
}

/// Container-agnostic index link of a tree
#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
pub struct Link<S> {
    slink: SiblingsLink<S>,
    clink: ChildLink<S>,
    parent: Option<S>,
}

/// Doubly linked list indices for siblings
#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
struct SiblingsLink<S> {
    /// Next sibling
    next: Option<S>,
    /// Previous sibling
    prev: Option<S>,
}

/// Indices for referring to children. NOTE: The first and last fields must not overlap.
#[derive(Debug, Default, Clone, PartialEq, Eq, Hash)]
struct ChildLink<S> {
    first: Option<S>,
    last: Option<S>,
}

impl<S: Clone> Link<S> {
    pub fn with_parent(parent: S) -> Self
    where
        S: Default,
    {
        Self {
            parent: Some(parent),
            ..Default::default()
        }
    }

    pub fn parent(&self) -> Option<S> {
        self.parent.clone()
    }

    pub fn set_parent(&mut self, parent: Option<S>) {
        self.parent = parent;
    }

    pub fn next_sibling(&self) -> Option<S> {
        self.slink.next.clone()
    }

    pub fn prev_sibling(&self) -> Option<S> {
        self.slink.prev.clone()
    }

    pub fn has_any_child(&self) -> bool {
        self.clink.first.is_some() || self.clink.last.is_some()
    }

    pub fn first_child(&self) -> Option<S> {
        self.clink.first.clone()
    }

    pub fn last_child(&self) -> Option<S> {
        self.clink.last.clone()
    }

    fn set_next_sibling(&mut self, next: Option<S>) {
        self.slink.next = next;
    }

    fn set_prev_sibling(&mut self, prev: Option<S>) {
        self.slink.prev = prev;
    }

    fn set_first_child(&mut self, first: Option<S>) {
        self.clink.first = first;
    }

    fn set_last_child(&mut self, last: Option<S>) {
        self.clink.last = last;
    }
}

pub fn fix_siblings_on_remove<T: Tree>(link: &Link<T::Slot>, tree: &mut T) {
    let slink = &link.slink;

    if let Some(prev) = slink.prev.clone() {
        let prev_link = tree.link_mut_by_slot(prev.clone()).unwrap();
        prev_link.set_next_sibling(slink.next.clone());
    }

    if let Some(next) = slink.next.clone() {
        let next_link = tree.link_mut_by_slot(next.clone()).unwrap();
        next_link.set_prev_sibling(slink.prev.clone());
    }
}

/// On tree node insertion
pub fn fix_root_on_insert<T: Tree>(tree: &mut T, id: T::Id) {
    let root = tree.root_mut();

    if root.first_child().is_none() {
        root.set_first_child(Some(id.slot()));
    } else {
        // NOTE: The first and last fields must not overlap.
        debug_assert_ne!(root.first_child(), root.last_child());

        if let Some(last_slot) = root.last_child().or(root.first_child()) {
            drop(root);

            let last_link = tree.link_mut_by_slot(last_slot.clone()).unwrap();
            debug_assert!(last_link.next_sibling().is_none());
            last_link.set_next_sibling(Some(id.slot()));
            let link = tree.link_mut_by_slot(id.slot()).unwrap();
            link.set_prev_sibling(Some(last_slot.clone()));
        }

        tree.root_mut().set_last_child(Some(id.slot()));
    }
}

pub fn fix_after_clean_children<S: Default>(link: &mut Link<S>) {
    link.clink = Default::default();
}

/// Fixes parent/child link on leaf node removal
pub(crate) fn fix_children_on_remove_leaf<T: Tree>(
    link: &Link<T::Slot>,
    child_slot: T::Slot,
    tree: &mut T,
) {
    let clink = &link.clink;

    // NOTE: The first and last fields must not overlap.
    debug_assert_ne!(clink.first, clink.last);

    // Fix the parent-child link
    if let Some(first_slot) = clink.first.clone() {
        if first_slot == child_slot {
            let first_link = tree.link_mut_by_slot(first_slot.clone()).unwrap();
            debug_assert!(first_link.prev_sibling().is_none());

            if let Some(second_slot) = first_link.next_sibling() {
                let second_link = tree.link_mut_by_slot(second_slot).unwrap();
                debug_assert!(second_link.prev_sibling() == Some(first_slot.clone()));
                second_link.set_prev_sibling(None);
            }

            return;
        }
    }

    if let Some(last_slot) = clink.last.clone() {
        if last_slot == child_slot {
            let last_link = tree.link_mut_by_slot(last_slot.clone()).unwrap();
            debug_assert!(last_link.next_sibling().is_none());

            if let Some(before_last_slot) = last_link.prev_sibling() {
                let before_last_link = tree.link_mut_by_slot(before_last_slot).unwrap();
                debug_assert!(before_last_link.next_sibling() == Some(last_slot.clone()));
                before_last_link.set_next_sibling(None);
            }

            return;
        }
    }
}

pub fn fix_on_attach<T: Tree>(parent_id: T::Id, child_id: T::Id, tree: &mut T) {
    let child_slot = child_id.slot();

    // siblings link
    let self_link = tree.link_mut_by_id(parent_id.clone()).unwrap();
    if let Some(last_slot) = self_link.last_child().or(self_link.first_child()) {
        let (last_link, child_link) = tree
            .link2_mut_by_slot(last_slot.clone(), child_slot.clone())
            .unwrap();
        debug_assert!(last_link.next_sibling().is_none());
        last_link.set_next_sibling(Some(child_slot.clone()));
        child_link.set_prev_sibling(Some(last_slot));
    }

    // parent -> child link
    let parent_link = tree.link_mut_by_id(parent_id.clone()).unwrap();

    if parent_link.first_child().is_none() {
        debug_assert!(parent_link.last_child().is_none());
        parent_link.set_first_child(Some(child_slot.clone()));
    } else {
        parent_link.set_last_child(Some(child_slot.clone()));
    }
}
