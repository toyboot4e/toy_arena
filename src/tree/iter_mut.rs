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
    /// Here we're using `UnsafeCell` instead of `RefCell` so that we don't have to be bothered with
    /// lifetime of `cell::{Ref, RefMut}`. How ever, it is our risk to not break the aliasing rules
    /// at runtime.
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
    pub unsafe fn tree(&self) -> &Tree<T, D, G> {
        unsafe { &*self.inner.as_ref().tree.get() }
    }

    /// # Safety
    /// It's backed by `UnsafeCell`. Make sure to follow the aliasing rule at runtime!
    pub unsafe fn tree_mut(&self) -> &mut Tree<T, D, G> {
        unsafe { &mut *self.inner.as_ref().tree.get() }
    }
}

/// Binding of an existing node and their children
#[derive(Derivative)]
#[derivative(Debug(bound = "T: Debug"))]
pub struct NodeMut<'a, T, D = (), G: Gen = DefaultGen> {
    #[derivative(Debug = "ignore")]
    pub(crate) bind: TreeBind<'a, T, D, G>,
    pub(crate) slot: Slot,
}

impl<'a, T, D, G: Gen> NodeMut<'a, T, D, G> {
    pub(crate) fn bind(tree: &'a mut Tree<T, D, G>, slot: Slot) -> Self {
        Self {
            bind: TreeBind::new(tree),
            slot,
        }
    }

    /// # Safety
    /// Panics if the data is removed/invaldated.
    pub fn id(&self) -> NodeId<T, D, G> {
        let tree = unsafe { self.bind.tree() };
        tree.nodes.upgrade(self.slot).unwrap()
    }

    /// # Safety
    /// Panics if the data is removed/invaldated.
    unsafe fn node(&self) -> &Node<T> {
        let tree = unsafe { self.bind.tree() };
        tree.nodes.get_by_slot(self.slot).unwrap()
    }

    /// # Safety
    /// Panics if the data is removed/invaldated.
    unsafe fn node_mut(&self) -> &mut Node<T> {
        let tree = unsafe { self.bind.tree_mut() };
        tree.nodes.get_mut_by_slot(self.slot).unwrap()
    }

    /// # Safety
    /// Panics if the data is removed/invaldated.
    pub fn data(&self) -> &T {
        let tree = unsafe { self.bind.tree() };
        tree.nodes.get_by_slot(self.slot).unwrap().data()
    }

    /// # Safety
    /// Panics if the data is removed/invaldated.
    pub fn data_mut(&mut self) -> &mut T {
        let tree = unsafe { self.bind.tree_mut() };
        tree.nodes.get_mut_by_slot(self.slot).unwrap().data_mut()
    }

    /// Removes subtree rooted by this node
    pub fn remove(&mut self) {
        let node = unsafe { self.node() };
        let slink = node.slink.clone();
        let parent = node.parent.clone();
        drop(node);

        // parent
        {
            let tree = unsafe { self.bind.tree_mut() };
            let clink = if let Some(parent) = parent {
                let parent = tree.node_mut_by_slot(parent).unwrap();
                parent.clink.clone()
            } else {
                tree.root.clone()
            };
            clink.remove(tree, self.slot);
        }

        // children
        self.remove_rec();

        // Fix the siblnig link
        if let Some(prev) = slink.prev {
            let tree = unsafe { self.bind.tree_mut() };
            let prev = tree.nodes.get_mut_by_slot(prev).unwrap();
            prev.slink.next = slink.next;
        }
        if let Some(next) = slink.next {
            let tree = unsafe { self.bind.tree_mut() };
            let next = tree.nodes.get_mut_by_slot(next).unwrap();
            next.slink.prev = slink.prev;
        }
    }

    fn remove_rec(&mut self) {
        for mut child in self.children_mut() {
            child.remove_rec();
        }
        // remove this node
        let tree = unsafe { self.bind.tree_mut() };
        tree.nodes.remove_by_slot(self.slot);
    }
}

/// # ---- Mutable iterators ----
impl<'a, T, D, G: Gen> NodeMut<'a, T, D, G> {
    // TODO: add immutable iterators

    /// Nodes after this node
    pub fn siblings_mut(&mut self) -> SiblingsMutNext<'a, T, D, G> {
        SiblingsMutNext {
            bind: self.bind.clone(),
            next: unsafe { self.node().slink.next },
        }
    }

    /// This node and nodes after this node
    pub fn preorder_mut(&mut self) -> SiblingsMutNext<'a, T, D, G> {
        SiblingsMutNext {
            bind: self.bind.clone(),
            next: Some(self.slot),
        }
    }

    /// Children of this node
    pub fn children_mut(&mut self) -> SiblingsMutNext<'a, T, D, G> {
        SiblingsMutNext {
            bind: self.bind.clone(),
            next: unsafe { self.node().clink.first },
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
    #[derivative(Debug = "ignore")]
    pub(crate) bind: TreeBind<'a, T, D, G>,
    pub(crate) next: Option<Slot>,
}

impl<'a, T, D, G: Gen> Iterator for SiblingsMutNext<'a, T, D, G> {
    type Item = NodeMut<'a, T, D, G>;
    fn next(&mut self) -> Option<Self::Item> {
        let next = self.next?;

        self.next = {
            let tree = unsafe { self.bind.tree() };
            let next_node = tree.nodes.get_by_slot(next).unwrap();
            debug_assert_ne!(self.next, next_node.slink.next);
            next_node.slink.next
        };

        Some(NodeMut {
            slot: next,
            bind: self.bind.clone(),
        })
    }
}
