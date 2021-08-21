/*!
Iterators of mutable bindings of tree nodes
*/

use std::{cell::UnsafeCell, mem, rc::Rc};

use super::*;

/// Virtual `Rc<RefCell<Tree>>`
pub(crate) struct TreeBind<'a, T, D = (), G: Gen = DefaultGen> {
    inner: Rc<TreeBindDrop<'a, T, D, G>>,
}

impl<'a, T, D, G: Gen> Clone for TreeBind<'a, T, D, G> {
    fn clone(&self) -> Self {
        Self {
            inner: Rc::clone(&self.inner),
        }
    }
}

struct TreeBindDrop<'a, T, D = (), G: Gen = DefaultGen> {
    original: &'a mut Tree<T, D, G>,
    tree: UnsafeCell<Tree<T, D, G>>,
}

/// Give back the tree on drop
impl<'a, T, D, G: Gen> Drop for TreeBindDrop<'a, T, D, G> {
    fn drop(&mut self) {
        mem::swap(self.original, unsafe { &mut *self.tree.get() });
    }
}

impl<'a, T, D, G: Gen> TreeBind<'a, T, D, G> {
    /// Take the tree and wrap it in `UnsafeCell`
    pub fn new(original: &'a mut Tree<T, D, G>) -> Self {
        let mut tree = Tree::new();
        mem::swap(original, &mut tree);

        Self {
            inner: Rc::new(TreeBindDrop {
                original,
                tree: UnsafeCell::new(tree),
            }),
        }
    }

    /// # Safety
    /// It's backed by `UnsafeCell`. Make sure to follow the aliasing rule at runtime!
    pub fn tree(&self) -> &Tree<T, D, G> {
        unsafe { &*self.inner.as_ref().tree.get() }
    }

    /// # Safety
    /// It's backed by `UnsafeCell`. Make sure to follow the aliasing rule at runtime!
    pub fn tree_mut(&self) -> &mut Tree<T, D, G> {
        unsafe { &mut *self.inner.as_ref().tree.get() }
    }
}

/// Reference to a node and their children
#[derive(Derivative)]
#[derivative(Debug(bound = "T: Debug"))]
pub struct NodeMut<'a, T, D = (), G: Gen = DefaultGen> {
    pub(crate) slot: Slot,
    #[derivative(Debug = "ignore")]
    pub(crate) bind: TreeBind<'a, T, D, G>,
}

impl<'a, T, D, G: Gen> NodeMut<'a, T, D, G> {
    pub fn id(&self) -> NodeId<T, D, G> {
        let tree = self.bind.tree();
        tree.nodes.upgrade(self.slot).unwrap()
    }

    pub fn node(&self) -> &Node<T> {
        let tree = self.bind.tree();
        tree.nodes.get_by_slot(self.slot).unwrap()
    }

    pub fn data(&self) -> &T {
        self.node().data()
    }

    /// Nodes after this node
    pub fn siblings(&self) -> SiblingsMutNext<'a, T, D, G> {
        SiblingsMutNext {
            next: self.node().slink.next,
            bind: self.bind.clone(),
        }
    }

    /// This node and nodes after this node
    pub fn preorder(&self) -> SiblingsMutNext<'a, T, D, G> {
        SiblingsMutNext {
            next: Some(self.slot),
            bind: self.bind.clone(),
        }
    }

    pub fn children(&self) -> SiblingsMutNext<'a, T, D, G> {
        SiblingsMutNext {
            next: self.node().clink.first,
            // we know
            bind: self.bind.clone(),
        }
    }
}

// --------------------------------------------------------------------------------
// Manual iteator
// - Hide `Slot` from user
// - Implement automatic iterator

/// Iterator that walks through siblings
// TODO: directon type parameter
#[derive(Derivative)]
#[derivative(Debug(bound = "T: Debug"))]
pub struct SiblingsMutNext<'a, T, D = (), G: Gen = DefaultGen> {
    pub(crate) next: Option<Slot>,
    #[derivative(Debug = "ignore")]
    pub(crate) bind: TreeBind<'a, T, D, G>,
}

impl<'a, T, D, G: Gen> Iterator for SiblingsMutNext<'a, T, D, G> {
    type Item = NodeMut<'a, T, D, G>;
    fn next(&mut self) -> Option<Self::Item> {
        let next = self.next?;

        self.next = {
            let tree = self.bind.tree();
            let next_node = tree.nodes.get_by_slot(next).unwrap();
            next_node.slink.next
        };

        Some(NodeMut {
            slot: next,
            bind: self.bind.clone(),
        })
    }
}
