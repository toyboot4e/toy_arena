/*!
Directed rooted tree, layered on top of [`Arena`]

DRT is useful for, for example, implementing scene graph of a game.
*/

pub mod iter;

#[cfg(test)]
mod test;

use derivative::Derivative;
use std::{fmt::Debug, hash::Hash, marker::PhantomData};

use crate::{Arena, DefaultGen, Gen};

/// [`Index`](crate::Index) of node in [`Drt`]
pub type Index<T, D = (), G = DefaultGen> = crate::Index<Node<T>, D, G>;

type Children<T> = Vec<T>;

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
    arena: Arena<Node<T>, D, G>,
    root: Node<T, D, G>,
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
    children: Children<Index<T, D, G>>,
    _distinct: PhantomData<D>,
}

impl<T, D, G: Gen> Drt<T, D, G> {
    pub fn root(&self) -> &Node<T, D, G> {
        &self.root
    }
}

// Delegated to the underlaying arena
impl<T, D, G: Gen> Drt<T, D, G> {
    pub fn contains(&self, index: Index<T, D, G>) -> bool {
        self.arena.contains(index)
    }

    pub fn len(&self) -> usize {
        self.arena.len()
    }
}

// Wrappers of arena operations
impl<T, D, G: Gen> Drt<T, D, G> {
    /// Appends a new adta to the root node
    pub fn insert(&mut self, token: T) -> Index<T, D, G> {
        self.root.append_impl(&mut self.arena, token)
    }
}

impl<T, D, G: Gen> Node<T, D, G> {
    fn new(token: T) -> Self {
        Self {
            token,
            children: Children::default(),
            _distinct: PhantomData,
        }
    }

    pub fn child_indices(&self) -> &[Index<T, D, G>] {
        &self.children
    }

    pub fn get(&self) -> &T {
        &self.token
    }

    pub fn get_mut(&mut self) -> &mut T {
        &mut self.token
    }

    /// Append child
    pub fn append(&mut self, drt: &mut Drt<T, D, G>, child: T) -> Index<T, D, G> {
        self.append_impl(&mut drt.arena, child)
    }

    fn append_impl(&mut self, arena: &mut Arena<Node<T>, D, G>, child: T) -> Index<T, D, G> {
        let node = Node::new(child);
        let index = arena.insert(node);
        self.children.push(index);
        index
    }
}
