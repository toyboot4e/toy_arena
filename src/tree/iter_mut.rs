//! Iterators of mutable bindings of the [`Tree`] nodes.

use std::{cell::UnsafeCell, mem, rc::Rc};

use super::*;

/// Virtual `Rc<RefCell<Tree>>`
pub(crate) struct TreeBind<'a, T, G: Gen = DefaultGen> {
    inner: Rc<TreeBindDrop<'a, T, G>>,
}

impl<'a, T, G: Gen> Clone for TreeBind<'a, T, G> {
    fn clone(&self) -> Self {
        Self {
            inner: Rc::clone(&self.inner),
        }
    }
}

struct TreeBindDrop<'a, T, G: Gen = DefaultGen> {
    original: &'a mut Tree<T, G>,
    /// Here we're using `UnsafeCell` instead of `RefCell` so that we don't have to be bothered with
    /// lifetime of `cell::{Ref, RefMut}`. How ever, it is our risk to not break the aliasing rules
    /// at runtime.
    tree: UnsafeCell<Tree<T, G>>,
}

/// Give back the tree on drop
impl<'a, T, G: Gen> Drop for TreeBindDrop<'a, T, G> {
    fn drop(&mut self) {
        mem::swap(self.original, unsafe { &mut *self.tree.get() });
    }
}

impl<'a, T, G: Gen> TreeBind<'a, T, G> {
    /// Take the tree and wrap it in `UnsafeCell`.
    pub fn new(original: &'a mut Tree<T, G>) -> Self {
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
    pub unsafe fn tree(&self) -> &Tree<T, G> {
        unsafe { &*self.inner.as_ref().tree.get() }
    }

    /// # Safety
    /// It's backed by `UnsafeCell`. Make sure to follow the aliasing rule at runtime!
    pub unsafe fn tree_mut(&self) -> &mut Tree<T, G> {
        unsafe { &mut *self.inner.as_ref().tree.get() }
    }
}

/// Binding of an existing node and their children
#[derive(Derivative)]
#[derivative(Debug(bound = "T: Debug"))]
pub struct NodeMut<'a, T, G: Gen = DefaultGen> {
    #[derivative(Debug = "ignore")]
    pub(crate) bind: TreeBind<'a, T, G>,
    pub(crate) slot: Slot,
}

impl<'a, T, G: Gen> NodeMut<'a, T, G> {
    pub(crate) fn bind(tree: &'a mut Tree<T, G>, slot: Slot) -> Self {
        Self {
            bind: TreeBind::new(tree),
            slot,
        }
    }

    pub fn id(&self) -> NodeId<T, G> {
        let tree = unsafe { self.bind.tree() };
        tree.nodes.upgrade(self.slot).unwrap()
    }

    unsafe fn node(&self) -> &Node<T, G> {
        let tree = unsafe { self.bind.tree() };
        tree.nodes.get_by_slot(self.slot).unwrap()
    }

    unsafe fn node_mut(&self) -> &mut Node<T, G> {
        let tree = unsafe { self.bind.tree_mut() };
        tree.nodes.get_mut_by_slot(self.slot).unwrap()
    }

    unsafe fn link(&self) -> &Link<Slot> {
        &self.node().link
    }

    unsafe fn link_mut(&mut self) -> &mut Link<Slot> {
        &mut self.node_mut().link
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
        let next = unsafe { self.link().next_sibling()? };
        Some(Self {
            bind: self.bind.clone(),
            slot: next,
        })
    }

    pub fn prev_sibling(&mut self) -> Option<Self> {
        let next = unsafe { self.link().next_sibling()? };
        Some(Self {
            bind: self.bind.clone(),
            slot: next,
        })
    }

    /// Goes back to the first sibling.
    pub fn first_sibling(&mut self) -> Option<Self> {
        let mut slot = self.slot;

        while let Some(next) = unsafe { self.link().prev_sibling() } {
            slot = next;
        }

        Some(Self {
            bind: self.bind.clone(),
            slot,
        })
    }

    /// Goes forward to the last sibling.
    pub fn last_sibling(&mut self) -> Option<Self> {
        let mut slot = self.slot;

        while let Some(next) = unsafe { self.link().next_sibling() } {
            slot = next;
        }

        Some(Self {
            bind: self.bind.clone(),
            slot,
        })
    }

    pub fn children(&mut self) -> SiblingsMutNext<'a, T, G> {
        let next = unsafe { self.link().first_child() };
        SiblingsMutNext {
            bind: self.bind.clone(),
            next,
        }
    }

    pub fn remove_children(&mut self) {
        for mut child in self.children_mut() {
            child.remove_children_rec();
        }
        unsafe {
            link::fix_after_clean_children(self.link_mut());
        }
    }

    fn remove_children_rec(&mut self) {
        for mut child in self.children_mut() {
            child.remove_children();
        }
        // Invalidate this child node WITHOUT fixing the parent-child link
        let tree = unsafe { self.bind.tree_mut() };
        tree.nodes.remove_by_slot(self.slot);
    }

    /// Removes subtree rooted by this node.
    // NOTE: It doesn't mutate the node's siblings link!
    pub fn remove(mut self) {
        let link = unsafe { self.link().clone() };

        // Fix parent-child link
        {
            let tree = unsafe { self.bind.tree_mut() };
            let parent_link = if let Some(parent) = link.parent() {
                let parent = tree.node_by_slot(parent).unwrap();
                parent.link.clone()
            } else {
                tree.root.clone()
            };
            link::fix_children_on_remove_leaf(&parent_link, self.slot, tree);
        }

        // Remove children
        self.remove_children();

        // Fix the siblnig link, but keeping the target node's link
        // so that the link can be used by the iterator
        let tree = unsafe { self.bind.tree_mut() };
        link::fix_siblings_on_remove(&link, tree);
    }
}

/// # ---- Iterators ----
impl<'a, T, G: Gen> NodeMut<'a, T, G> {
    /// This node and nodes after this node.
    pub fn traverse_mut(&mut self) -> SiblingsMutNext<'a, T, G> {
        SiblingsMutNext {
            bind: self.bind.clone(),
            next: Some(self.slot),
        }
    }

    /// Nodes after this node.
    pub fn siblings_mut(&mut self) -> SiblingsMutNext<'a, T, G> {
        SiblingsMutNext {
            bind: self.bind.clone(),
            next: unsafe { self.link().next_sibling() },
        }
    }

    // /// Sub tree rooted at the node (depth-first, preorder)
    // pub fn subtree(&self, id: NodeId<T,D,G>) -> iter::Traverse<T,D,G> {
    //     let states = vec![iter::TraverseState::Parent(iter::NodeRef::new(
    //         id.slot, self,
    //     ))];
    //     iter::Traverse { tree: self, states }
    // }

    /// Children of this node.
    pub fn children_mut(&mut self) -> SiblingsMutNext<'a, T, G> {
        SiblingsMutNext {
            bind: self.bind.clone(),
            next: unsafe { self.link().first_child() },
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
pub struct SiblingsMutNext<'a, T, G: Gen = DefaultGen> {
    #[derivative(Debug = "ignore")]
    pub(crate) bind: TreeBind<'a, T, G>,
    pub(crate) next: Option<Slot>,
}

impl<'a, T, G: Gen> SiblingsMutNext<'a, T, G> {
    pub fn bind(&mut self) -> Option<NodeMut<'a, T, G>> {
        if let Some(slot) = self.next {
            Some(NodeMut {
                bind: self.bind.clone(),
                slot,
            })
        } else {
            None
        }
    }
}

impl<'a, T, G: Gen> Iterator for SiblingsMutNext<'a, T, G> {
    type Item = NodeMut<'a, T, G>;
    fn next(&mut self) -> Option<Self::Item> {
        let next = self.next?;

        self.next = {
            let tree = unsafe { self.bind.tree() };
            let next_node = tree.nodes.get_by_slot(next).unwrap();
            debug_assert_ne!(self.next, next_node.link.next_sibling());
            next_node.link.next_sibling()
        };

        Some(NodeMut {
            slot: next,
            bind: self.bind.clone(),
        })
    }
}
