/*!
Iterators of mutable bindings of the [`Tree`] nodes
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

    pub fn id(&self) -> NodeId<T, D, G> {
        let tree = unsafe { self.bind.tree() };
        tree.nodes.upgrade(self.slot).unwrap()
    }

    unsafe fn node(&self) -> &Node<T> {
        let tree = unsafe { self.bind.tree() };
        tree.nodes.get_by_slot(self.slot).unwrap()
    }

    unsafe fn node_mut(&self) -> &mut Node<T> {
        let tree = unsafe { self.bind.tree_mut() };
        tree.nodes.get_mut_by_slot(self.slot).unwrap()
    }

    pub fn data(&self) -> &T {
        let tree = unsafe { self.bind.tree() };
        tree.nodes.get_by_slot(self.slot).unwrap().data()
    }

    pub fn data_mut(&mut self) -> &mut T {
        let tree = unsafe { self.bind.tree_mut() };
        tree.nodes.get_mut_by_slot(self.slot).unwrap().data_mut()
    }

    pub fn next_sibling(&mut self) -> Option<Self> {
        let next = {
            let tree = unsafe { self.bind.tree_mut() };
            let node = tree.nodes.get_by_slot(self.slot).unwrap();
            node.slink.next?
        };
        Some(Self {
            bind: self.bind.clone(),
            slot: next,
        })
    }

    pub fn prev_sibling(&mut self) -> Option<Self> {
        let next = {
            let tree = unsafe { self.bind.tree_mut() };
            let node = tree.nodes.get_by_slot(self.slot).unwrap();
            node.slink.next?
        };
        Some(Self {
            bind: self.bind.clone(),
            slot: next,
        })
    }

    pub fn remove_children(&mut self) {
        for mut child in self.children_mut() {
            child.remove_children();
        }
        // Invalidate this node WITHOUT fixing the parent-child link
        let tree = unsafe { self.bind.tree_mut() };
        tree.nodes.remove_by_slot(self.slot);
    }

    /// Removes subtree rooted by this node
    // NOTE: It doesn't mutate the node's siblings link!
    pub fn remove(mut self) {
        let (slink, parent) = {
            let node = unsafe { self.node() };
            (node.slink.clone(), node.parent.clone())
        };

        // Fix parent-child link
        {
            let tree = unsafe { self.bind.tree_mut() };
            let parent_clink = if let Some(parent) = parent {
                let parent = tree.node_mut_by_slot(parent).unwrap();
                parent.clink.clone()
            } else {
                tree.root.clone()
            };
            parent_clink.on_remove_leaf(self.slot, tree);
        }

        // Remove children
        self.remove_children();

        // Fix the siblnig link, but keeping the target node's link
        // so that the link can be used by the iterator
        let tree = unsafe { self.bind.tree_mut() };
        slink.on_remove(tree);
    }
}

/// # ---- Iterators ----
impl<'a, T, D, G: Gen> NodeMut<'a, T, D, G> {
    /// This node and nodes after this node
    pub fn traverse_mut(&mut self) -> SiblingsMutNext<'a, T, D, G> {
        SiblingsMutNext {
            bind: self.bind.clone(),
            next: Some(self.slot),
        }
    }

    /// Nodes after this node
    pub fn siblings_mut(&mut self) -> SiblingsMutNext<'a, T, D, G> {
        SiblingsMutNext {
            bind: self.bind.clone(),
            next: unsafe { self.node().slink.next },
        }
    }

    // /// Sub tree rooted at the node (depth-first, preorder)
    // pub fn subtree(&self, id: NodeId<T,D,G>) -> iter::Traverse<T,D,G> {
    //     let states = vec![iter::TraverseState::Parent(iter::NodeRef::new(
    //         id.slot, self,
    //     ))];
    //     iter::Traverse { tree: self, states }
    // }

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
