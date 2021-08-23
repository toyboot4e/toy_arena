/*!
Rooted tree layered on top of the generational arena
*/

pub mod iter;
pub mod iter_mut;

#[cfg(test)]
mod test;

pub use iter::TraverseItem;

use std::{fmt::Debug, hash::Hash, ops};

use derivative::Derivative;

use crate::{DefaultGen, Gen, Slot};

/**
[`Index`](crate::Index) of node in [`Tree`]
*/
pub type NodeId<T, D = (), G = DefaultGen> = crate::Index<Node<T>, D, G>;

type NodeArena<T, D, G> = crate::Arena<Node<T>, D, G>;

// TODO: deep clone

/**
Non-rooted tree layered on top of [`Arena`](crate::Arena). See [`NodeId`] for parenting methods.
*/
#[derive(Derivative)]
#[derivative(
    Debug(bound = "T: Debug"),
    PartialEq(bound = "T: PartialEq"),
    Eq(bound = "T: PartialEq"),
    Hash(bound = "T: Hash")
)]
pub struct Tree<T, D = (), G: Gen = DefaultGen> {
    nodes: NodeArena<T, D, G>,
    /// Corresponds to the implicit root
    root: ChildLink,
}

/// Tree node
#[derive(Derivative)]
#[derivative(
    Debug(bound = "T: Debug"),
    PartialEq(bound = "T: PartialEq"),
    Eq(bound = "T: Eq"),
    Hash(bound = "T: Hash")
)]
pub struct Node<T> {
    token: T,
    slink: SiblingLink,
    clink: ChildLink,
    /// `None` refers to the implicit root node. Parent handle is needed when removing the node
    /// later.
    parent: Option<Slot>,
}

/// Doubly linked list indices for siblings
#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
struct SiblingLink {
    // TODO use a nonmax type for slots
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

impl ChildLink {
    pub fn remove<'a, T, D, G: Gen>(&self, tree: &'a mut Tree<T, D, G>, slot: Slot) {
        // NOTE: The first and last fields must not overlap.
        debug_assert!(self.first != self.last);

        if let Some(first_slot) = self.first {
            if first_slot == slot {
                let first = tree.node_by_slot(first_slot).unwrap();
                debug_assert!(first.slink.prev.is_none());

                if let Some(next) = first.slink.next {
                    let next_node = tree.node_mut_by_slot(next).unwrap();
                    debug_assert!(next_node.slink.prev == Some(first_slot));
                    next_node.slink.prev = None;
                }

                return;
            }
        }

        if let Some(last_slot) = self.last {
            if last_slot != slot {
                let last = tree.node_by_slot(last_slot).unwrap();
                debug_assert!(last.slink.next.is_none());

                if let Some(prev_slot) = last.slink.prev {
                    let prev_node = tree.node_mut_by_slot(prev_slot).unwrap();
                    debug_assert!(prev_node.slink.next == Some(last_slot));
                    prev_node.slink.next = None;
                }

                return;
            }
        }
    }
}

impl<T, D, G: Gen> Default for Tree<T, D, G> {
    fn default() -> Self {
        Self::with_capacity(0)
    }
}

impl<T, D, G: Gen> Tree<T, D, G> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn with_capacity(cap: usize) -> Self {
        Self {
            nodes: NodeArena::with_capacity(cap),
            root: Default::default(),
        }
    }

    pub fn contains(&self, index: NodeId<T, D, G>) -> bool {
        self.nodes.contains(index)
    }

    /// Number of items in this tree
    pub fn len(&self) -> usize {
        self.nodes.len()
    }

    /// The capacity of the backgin vec
    pub fn capacity(&self) -> usize {
        self.nodes.capacity()
    }

    /// Returns a reference to the node
    pub fn node(&self, id: NodeId<T, D, G>) -> Option<&Node<T>> {
        self.nodes.get(id)
    }

    /// Returns a mutable reference to the node
    pub fn node_mut(&mut self, id: NodeId<T, D, G>) -> Option<&mut Node<T>> {
        self.nodes.get_mut(id)
    }

    /// Returns a reference to the node
    pub(crate) fn node_by_slot(&self, slot: Slot) -> Option<&Node<T>> {
        self.nodes.get_by_slot(slot)
    }

    /// Returns a mutable reference to the node
    pub(crate) fn node_mut_by_slot(&mut self, slot: Slot) -> Option<&mut Node<T>> {
        self.nodes.get_mut_by_slot(slot)
    }

    /// Returns reference to the data in a node
    pub fn data(&self, id: NodeId<T, D, G>) -> Option<&T> {
        self.nodes.get(id).map(Node::data)
    }

    /// Returns mutable reference to the data in a node
    pub fn data_mut(&mut self, id: NodeId<T, D, G>) -> Option<&mut T> {
        self.nodes.get_mut(id).map(Node::data_mut)
    }

    /// Appends a new data to the implicit root node
    pub fn insert(&mut self, token: T) -> NodeId<T, D, G> {
        let node = Node::new(token, None);
        let id = self.nodes.insert(node);

        // NOTE: The first and last fields must not overlap.
        self.root.last = Some(id.slot());
        if self.root.first.is_none() {
            self.root.first = Some(id.slot());
        }

        id
    }

    pub fn remove(&mut self, id: NodeId<T, D, G>) -> bool {
        match self.bind(id) {
            Some(mut bind) => {
                bind.remove();
                true
            }
            None => false,
        }
    }

    pub fn bind<'a>(&'a mut self, id: NodeId<T, D, G>) -> Option<iter_mut::NodeMut<'a, T, D, G>> {
        if !self.nodes.contains(id) {
            None
        } else {
            Some(iter_mut::NodeMut::bind(self, id.slot))
        }
    }
}

/// Iterators
/// ---
impl<T, D, G: Gen> Tree<T, D, G> {
    /// Depth-first search
    pub fn traverse(&self, id: NodeId<T, D, G>) -> iter::Traverse<T, D, G> {
        let states = vec![iter::TraverseState::Parent(iter::NodeRef::new(
            id.slot, self,
        ))];
        iter::Traverse { tree: self, states }
    }

    /// Children of the implicit root node
    pub fn rooted_nodes(&self) -> iter::SiblingsNext<T, D, G> {
        iter::SiblingsNext {
            next: self.root.first,
            tree: self,
        }
    }

    /// Depth-first search
    pub fn root_traverse(&self) -> iter::Traverse<T, D, G> {
        let mut states = Vec::new();
        if let Some(first) = self.root.first {
            states.push(iter::TraverseState::ImplicitRootChildren(
                iter::SiblingsNext {
                    next: Some(first),
                    tree: self,
                },
            ))
        };
        iter::Traverse { tree: self, states }
    }
}

impl<T, D, G: Gen> ops::Index<NodeId<T, D, G>> for Tree<T, D, G> {
    type Output = Node<T>;
    fn index(&self, id: NodeId<T, D, G>) -> &Self::Output {
        self.node(id).unwrap()
    }
}

impl<T, D, G: Gen> ops::IndexMut<NodeId<T, D, G>> for Tree<T, D, G> {
    fn index_mut(&mut self, id: NodeId<T, D, G>) -> &mut Self::Output {
        self.node_mut(id).unwrap()
    }
}

impl<T> Node<T> {
    fn new(token: T, parent: Option<Slot>) -> Self {
        Self {
            token,
            clink: Default::default(),
            slink: Default::default(),
            parent,
        }
    }

    /// Returns reference to the internal data
    pub fn data(&self) -> &T {
        &self.token
    }

    /// Returns mutable reference to the internal data
    pub fn data_mut(&mut self) -> &mut T {
        &mut self.token
    }
}

/// Implementation for DRT node index
impl<T, D, G: Gen> NodeId<T, D, G> {
    /// Attach child to the node
    pub fn attach(self, tree: &mut Tree<T, D, G>, child: T) -> Option<NodeId<T, D, G>> {
        if !tree.contains(self) {
            return None;
        };

        let child_node = Node::new(child, Some(self.slot));
        let child_id = tree.nodes.insert(child_node);
        let child_slot = child_id.slot();

        // connect the last child and the new child
        let self_node = tree.node_mut(self).unwrap();
        if let Some(last_slot) = self_node.clink.last {
            let (last_node, child_node) =
                tree.nodes.get2_mut_by_slot(last_slot, child_slot).unwrap();
            debug_assert!(last_node.slink.next.is_none());
            last_node.slink.next = Some(child_slot);
            child_node.slink.prev = Some(last_slot);
        }

        // append the new child
        let self_node = tree.node_mut(self).unwrap();
        self_node.clink.last = Some(child_slot);

        if self_node.clink.first.is_none() {
            self_node.clink.first = self_node.clink.last;
        }

        Some(child_id)
    }

    // /// Detach child from the node
    // pub fn detach(
    //     self,
    //     tree: &mut Tree<T, D, G>,
    //     child: NodeId<T, D, G>,
    // ) -> Option<NodeId<T, D, G>> {
    //     let child = tree.node(child)?;

    //     Some(child)
    // }

    /// Returns iterator of child node bindings
    pub fn children_mut(self, tree: &mut Tree<T, D, G>) -> iter_mut::SiblingsMutNext<T, D, G> {
        let first = tree.node(self).and_then(|node| node.clink.first);
        let bind = iter_mut::TreeBind::new(tree);
        iter_mut::SiblingsMutNext { bind, next: first }
    }
}
