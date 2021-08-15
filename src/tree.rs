/*!
Rooted tree of sibling/children-aware nodes
*/

pub mod iter;

#[cfg(test)]
mod test;

pub use iter::TraverseItem;

use std::{fmt::Debug, hash::Hash, ops};

use derivative::Derivative;

use crate::{DefaultGen, Gen, Slot};

/// [`Index`](crate::Index) of node in [`Tree`]
pub type NodeId<T, D = (), G = DefaultGen> = crate::Index<Node<T>, D, G>;

type NodeArena<T, D, G> = crate::Arena<Node<T>, D, G>;

// TODO: deep clone

/// Non-rooted tree layered on top of [`Arena`](crate::Arena). Parenting methods are defined as
/// [`NodeId`] methods.
#[derive(Derivative)]
#[derivative(
    Debug(bound = "T: Debug"),
    PartialEq(bound = "T: PartialEq"),
    Eq(bound = "T: PartialEq"),
    Hash(bound = "T: Hash")
)]
pub struct Tree<T, D = (), G: Gen = DefaultGen> {
    nodes: NodeArena<T, D, G>,
    /// Corresponds to implicit root
    clink: ChildLink,
}

/// Node that is aware of siblings and chidren
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
}

/// Siblings are connected as linked list
#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
struct SiblingLink {
    /// Next sibling
    next: Option<Slot>,
    /// Previous sibling
    prev: Option<Slot>,
}

/// Children are also linked list and the start and end is referred to
#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
struct ChildLink {
    first: Option<Slot>,
    last: Option<Slot>,
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
            clink: Default::default(),
        }
    }

    pub fn contains(&self, index: NodeId<T, D, G>) -> bool {
        self.nodes.contains(index)
    }

    pub fn len(&self) -> usize {
        self.nodes.len()
    }

    /// Appends a new data to the root node
    pub fn insert(&mut self, token: T) -> NodeId<T, D, G> {
        let node = Node::new(token);
        let id = self.nodes.insert(node);

        self.clink.last = Some(id.slot());
        if self.clink.first.is_none() {
            self.clink.first = Some(id.slot());
        }

        id
    }

    pub fn get(&self, id: NodeId<T, D, G>) -> Option<&Node<T>> {
        self.nodes.get(id)
    }

    pub fn get_mut(&mut self, id: NodeId<T, D, G>) -> Option<&mut Node<T>> {
        self.nodes.get_mut(id)
    }
}

/// Iterators
/// ---
impl<T, D, G: Gen> Tree<T, D, G> {
    /// Depth-first search
    pub fn traverse(&self, id: NodeId<T, D, G>) -> iter::Traverse<T, D, G> {
        let states = vec![iter::TraverseState::Parent(iter::NodeRef {
            slot: id.slot,
            tree: self,
        })];
        // let states = vec![iter::TraverseState {
        //     node: iter::NodeRef {
        //         slot: id.slot,
        //         tree: self,
        //     },
        //     tag: iter::TraverseStateTag::Parent,
        // }];
        iter::Traverse { tree: self, states }
    }

    /// Children of the implicit root node
    pub fn rooted_nodes(&self) -> iter::SiblingsNext<T, D, G> {
        iter::SiblingsNext {
            next: self.clink.first,
            tree: self,
        }
    }

    /// Depth-first search
    pub fn root_traverse(&self) -> iter::Traverse<T, D, G> {
        let mut states = Vec::new();
        if let Some(first) = self.clink.first {
            states.push(iter::TraverseState::ImplicitRootChildren(
                iter::SiblingsNext {
                    next: Some(first),
                    tree: self,
                },
            ))
            // states.push(iter::TraverseState {
            //     node: iter::NodeRef {
            //         slot: first,
            //         tree: self,
            //     },
            //     tag: iter::TraverseStateTag::ImplicitRootChildren,
            // });
        };
        iter::Traverse { tree: self, states }
    }
}

impl<T, D, G: Gen> ops::Index<NodeId<T, D, G>> for Tree<T, D, G> {
    type Output = Node<T>;
    fn index(&self, id: NodeId<T, D, G>) -> &Self::Output {
        self.get(id).unwrap()
    }
}

impl<T, D, G: Gen> ops::IndexMut<NodeId<T, D, G>> for Tree<T, D, G> {
    fn index_mut(&mut self, id: NodeId<T, D, G>) -> &mut Self::Output {
        self.get_mut(id).unwrap()
    }
}

impl<T> Node<T> {
    fn new(token: T) -> Self {
        Self {
            token,
            clink: Default::default(),
            slink: Default::default(),
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
    /// Append child
    pub fn append(self, tree: &mut Tree<T, D, G>, child: T) -> Option<NodeId<T, D, G>> {
        if !tree.contains(self) {
            return None;
        };

        let child_node = Node::new(child);
        let child_id = tree.nodes.insert(child_node);
        let child_slot = child_id.slot();

        // connect the last child and the new child
        let self_node = tree.get_mut(self).unwrap();
        if let Some(last_slot) = self_node.clink.last {
            let (last_node, child_node) =
                tree.nodes.get2_mut_by_slot(last_slot, child_slot).unwrap();
            debug_assert!(last_node.slink.next.is_none());
            last_node.slink.next = Some(child_slot);
            child_node.slink.prev = Some(last_slot);
        }

        // append the new child
        let self_node = tree.get_mut(self).unwrap();
        self_node.clink.last = Some(child_slot);

        if self_node.clink.first.is_none() {
            self_node.clink.first = self_node.clink.last;
        }

        Some(child_id)
    }

    // Remove this node and the children

    // Remove child and their children
}
