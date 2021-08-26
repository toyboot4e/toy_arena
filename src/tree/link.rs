/*!
Link of parent/child and siblings
*/

// TODO use a nonmax type for slots

use crate::{tree::Tree, Gen, Slot};

#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
pub(crate) struct Link {
    slink: SiblingsLink,
    clink: ChildLink,
    parent: Option<Slot>,
}

/// Doubly linked list indices for siblings
#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
struct SiblingsLink {
    /// Next sibling
    next: Option<Slot>,
    /// Previous sibling
    prev: Option<Slot>,
}

/// Indices for referring to children. NOTE: The first and last fields must not overlap.
#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
struct ChildLink {
    first: Option<Slot>,
    last: Option<Slot>,
}

impl Link {
    pub fn with_parent(parent: Slot) -> Self {
        Self {
            parent: Some(parent),
            ..Default::default()
        }
    }

    pub fn parent(&self) -> Option<Slot> {
        self.parent
    }

    pub fn set_parent(&mut self, parent: Option<Slot>) {
        self.parent = parent;
    }

    pub fn next_sibling(&self) -> Option<Slot> {
        self.slink.next
    }

    pub fn prev_sibling(&self) -> Option<Slot> {
        self.slink.prev
    }

    pub fn has_any_child(&self) -> bool {
        self.clink.first.is_some() || self.clink.last.is_some()
    }

    pub fn first_child(&self) -> Option<Slot> {
        self.clink.first
    }

    pub fn last_child(&self) -> Option<Slot> {
        self.clink.last
    }

    pub(crate) fn set_next_sibling(&mut self, next: Option<Slot>) {
        self.slink.next = next;
    }

    pub(crate) fn set_prev_sibling(&mut self, prev: Option<Slot>) {
        self.slink.prev = prev;
    }

    pub(crate) fn set_first_child(&mut self, first: Option<Slot>) {
        self.clink.first = first;
    }

    pub(crate) fn set_last_child(&mut self, last: Option<Slot>) {
        self.clink.last = last;
    }
}

pub(crate) fn fix_siblings_on_remove<T, D, G: Gen>(link: &Link, tree: &mut Tree<T, D, G>) {
    let slink = &link.slink;
    if let Some(prev) = slink.prev {
        let prev_node = tree.nodes.get_mut_by_slot(prev).unwrap();
        prev_node.link.set_next_sibling(slink.next);
    }
    if let Some(next) = slink.next {
        let next_node = tree.nodes.get_mut_by_slot(next).unwrap();
        next_node.link.set_prev_sibling(slink.prev);
    }
}

/// Fixes parent/child link on leaf node removal
pub(crate) fn fix_children_on_remove_leaf<T, D, G: Gen>(
    link: &Link,
    child_slot: Slot,
    tree: &mut Tree<T, D, G>,
) {
    let clink = &link.clink;

    // NOTE: The first and last fields must not overlap.
    debug_assert_ne!(clink.first, clink.last);

    // Fix the parent-child link
    if let Some(first_slot) = clink.first {
        if first_slot == child_slot {
            let first_node = tree.node_by_slot(first_slot).unwrap();
            debug_assert!(first_node.link.prev_sibling().is_none());

            if let Some(second_slot) = first_node.link.next_sibling() {
                let second_node = tree.node_mut_by_slot(second_slot).unwrap();
                debug_assert!(second_node.link.prev_sibling() == Some(first_slot));
                second_node.link.set_prev_sibling(None);
            }

            return;
        }
    }

    if let Some(last_slot) = clink.last {
        if last_slot == child_slot {
            let last_node = tree.node_by_slot(last_slot).unwrap();
            debug_assert!(last_node.link.next_sibling().is_none());

            if let Some(before_last_slot) = last_node.link.prev_sibling() {
                let before_last_node = tree.node_mut_by_slot(before_last_slot).unwrap();
                debug_assert!(before_last_node.link.next_sibling() == Some(last_slot));
                before_last_node.link.set_next_sibling(None);
            }

            return;
        }
    }
}
