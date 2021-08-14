/*!
Directed rooted tree, layered on top of [`Arena`]

DRT is useful for, for example, implementing scene graph of a game.
*/

pub mod iter;

#[cfg(test)]
mod test;

use std::{fmt::Debug, hash::Hash, ops};

use derivative::Derivative;

use crate::{DefaultGen, Gen, Slot};

/// [`Index`](crate::Index) of node in [`Drt`]
pub type NodeId<T, D = (), G = DefaultGen> = crate::Index<Node<T>, D, G>;

type NodeArena<T, D, G> = crate::Arena<Node<T>, D, G>;

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
    // TODO: use nonmax type?
}

/// Item with indices for linked list
#[derive(Derivative)]
#[derivative(
    Debug(bound = "T: Debug"),
    PartialEq(bound = "T: PartialEq"),
    Eq(bound = "T: Eq"),
    Hash(bound = "T: Hash")
)]
pub struct Node<T> {
    token: T,
    first_child: Option<Slot>,
    last_child: Option<Slot>,
    /// Next slibling
    next: Option<Slot>,
    /// Previous slibling
    prev: Option<Slot>,
}

impl<T, D, G: Gen> Default for Drt<T, D, G> {
    fn default() -> Self {
        Self::with_capacity(0)
    }
}

impl<T, D, G: Gen> Drt<T, D, G> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn with_capacity(cap: usize) -> Self {
        Self {
            nodes: NodeArena::with_capacity(cap),
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
        id
    }

    pub fn get(&self, id: NodeId<T, D, G>) -> Option<&Node<T>> {
        self.nodes.get(id)
    }

    pub fn get_mut(&mut self, id: NodeId<T, D, G>) -> Option<&mut Node<T>> {
        self.nodes.get_mut(id)
    }
}

impl<T, D, G: Gen> ops::Index<NodeId<T, D, G>> for Drt<T, D, G> {
    type Output = Node<T>;
    fn index(&self, id: NodeId<T, D, G>) -> &Self::Output {
        self.get(id).unwrap()
    }
}

impl<T, D, G: Gen> ops::IndexMut<NodeId<T, D, G>> for Drt<T, D, G> {
    fn index_mut(&mut self, id: NodeId<T, D, G>) -> &mut Self::Output {
        self.get_mut(id).unwrap()
    }
}

impl<T> Node<T> {
    fn new(token: T) -> Self {
        Self {
            token,
            first_child: None,
            last_child: None,
            next: None,
            prev: None,
        }
    }

    pub fn children<'a, D, G: Gen>(self, drt: &'a Drt<T, D, G>) -> iter::Children<'a, T, D, G> {
        todo!()
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
    pub fn append(self, drt: &mut Drt<T, D, G>, child: T) -> Option<NodeId<T, D, G>> {
        if !drt.nodes.contains(self) {
            return None;
        }

        let node = Node::new(child);
        let id = drt.nodes.insert(node);

        if id.slot() > Slot::ZERO {
            let slot = Slot {
                raw: id.slot().raw - 1,
            };
            let prev = &mut drt.nodes.entries[slot.raw as usize].data.unwrap();
            prev.next = Some(id);
        }

        let me = &mut drt.nodes[self];
        me.children.push(id);

        Some(id)
    }
}
