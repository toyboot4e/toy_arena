/*!
Directed rooted tree, layered on top of [`Arena`]

DRT is useful for, for example, implementing scene graph of a game.
*/

pub mod iter;

#[cfg(test)]
mod test;

use std::{fmt::Debug, hash::Hash, ops};

use derivative::Derivative;

use crate::{DefaultGen, Gen};

/// [`Index`](crate::Index) of node in [`Drt`]
pub type NodeId<T, D = (), G = DefaultGen> = crate::Index<Node<T, D, G>, D, G>;

type NodeArena<T, D, G> = crate::Arena<Node<T, D, G>, D, G>;
type Children<T, D, G> = Vec<NodeId<T, D, G>>;

// TODO: deep clone

/// Directed rooted tree, layered on top of [`Arena`]
#[derive(Derivative)]
#[derivative(
    Debug(bound = "T: Debug"),
    PartialEq(bound = "T: PartialEq"),
    Eq(bound = "T: PartialEq"),
    Hash(bound = "T: Hash")
)]
pub struct Drt<T, D = (), G: Gen = DefaultGen> {
    nodes: NodeArena<T, D, G>,
    root: Children<T, D, G>,
}

/// Item + children
#[derive(Derivative)]
#[derivative(
    Debug(bound = "T: Debug"),
    PartialEq(bound = "T: PartialEq"),
    Eq(bound = "T: Eq"),
    Hash(bound = "T: Hash")
)]
pub struct Node<T, D = (), G: Gen = DefaultGen> {
    token: T,
    children: Children<T, D, G>,
}

impl<T, D, G: Gen> Default for Drt<T, D, G> {
    fn default() -> Self {
        Self::with_capacity(4)
    }
}

impl<T, D, G: Gen> Drt<T, D, G> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn with_capacity(cap: usize) -> Self {
        Self {
            nodes: NodeArena::with_capacity(cap),
            root: Default::default(),
        }
    }

    /// Indices of children of the hidden root node
    pub fn root(&self) -> &[NodeId<T, D, G>] {
        &self.root
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
        self.root.push(id);
        id
    }

    pub fn get(&self, id: NodeId<T, D, G>) -> Option<&Node<T, D, G>> {
        self.nodes.get(id)
    }

    pub fn get_mut(&mut self, id: NodeId<T, D, G>) -> Option<&mut Node<T, D, G>> {
        self.nodes.get_mut(id)
    }
}

impl<T, D, G: Gen> ops::Index<NodeId<T, D, G>> for Drt<T, D, G> {
    type Output = Node<T, D, G>;
    fn index(&self, id: NodeId<T, D, G>) -> &Self::Output {
        self.get(id).unwrap()
    }
}

impl<T, D, G: Gen> ops::IndexMut<NodeId<T, D, G>> for Drt<T, D, G> {
    fn index_mut(&mut self, id: NodeId<T, D, G>) -> &mut Self::Output {
        self.get_mut(id).unwrap()
    }
}

impl<T, D, G: Gen> Node<T, D, G> {
    fn new(token: T) -> Self {
        Self {
            token,
            children: Children::default(),
        }
    }

    pub fn child_indices(&self) -> &[NodeId<T, D, G>] {
        &self.children
    }

    pub fn data(&self) -> &T {
        &self.token
    }

    pub fn data_mut(&mut self) -> &mut T {
        &mut self.token
    }
}

/// Implementation for DRT node index
impl<T, D, G: Gen> NodeId<T, D, G> {
    /// Append child
    pub fn append(self, drt: &mut Drt<T, D, G>, child: T) -> Option<NodeId<T, D, G>> {
        if !drt.nodes.contains(self) {
            return None;
        }

        let node = Node::new(child);
        let id = drt.nodes.insert(node);

        let me = &mut drt.nodes[self];
        me.children.push(id);

        Some(id)
    }
}
