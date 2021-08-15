/*!
Iterator types for [`Drt`]
*/

use std::{iter, marker::PhantomData};

use derivative::Derivative;

use super::*;

// macro_rules! impl_iter {
//     ($name:ident, $next:expr) => {
//         impl<'a, T, D, G> Iterator for $name<'a, T, D, G> {
//             type Item = &'a Node<T, D, G>;
//
//             fn next(&mut self) -> Option<Self::Item> {
//                 let node = tree.nodes[self.slot.raw as usize].data.as_mut().unwrap();
//                 $next(node, &self.tree)
//             }
//         }
//
//         impl<'a, T> core::iter::FusedIterator for $name<'a, T> {}
//     };
// }

// --------------------------------------------------------------------------------
// Manual iteators, mainly for hinding `Slot` from user

/// Iterator that walks through siblings
// TODO: directon type parameter
#[derive(Derivative)]
#[derivative(Debug(bound = "T: Debug"))]
pub struct SiblingsNext<'a, T, D = (), G: Gen = DefaultGen> {
    pub(crate) next: Option<Slot>,
    #[derivative(Debug = "ignore")]
    pub(crate) tree: &'a Tree<T, D, G>,
}

impl<'a, T, D, G: Gen> Iterator for SiblingsNext<'a, T, D, G> {
    type Item = NodeRef<'a, T, D, G>;
    fn next(&mut self) -> Option<Self::Item> {
        let next = self.next?;
        let next_node = self.tree.nodes.get_by_slot(next).unwrap();
        self.next = next_node.slink.next;
        Some(NodeRef {
            slot: next,
            tree: self.tree,
        })
    }
}

/// Reference to a node and their children
#[derive(Derivative)]
#[derivative(Debug(bound = "T: Debug"))]
pub struct NodeRef<'a, T, D = (), G: Gen = DefaultGen> {
    pub(crate) slot: Slot,
    #[derivative(Debug = "ignore")]
    pub(crate) tree: &'a Tree<T, D, G>,
}

impl<'a, T, D, G: Gen> NodeRef<'a, T, D, G> {
    pub fn id(&self) -> NodeId<T, D, G> {
        unsafe { self.tree.nodes.upgrade(self.slot).unwrap() }
    }

    pub fn node(&self) -> &'a Node<T> {
        self.tree.nodes.get_by_slot(self.slot).unwrap()
    }

    pub fn data(&self) -> &'a T {
        self.node().data()
    }

    /// Nodes after this node
    pub fn siblings(&self) -> SiblingsNext<'a, T, D, G> {
        SiblingsNext {
            next: self.node().slink.next,
            tree: self.tree,
        }
    }

    /// This node and nodes after this node
    pub fn preorder(&self) -> SiblingsNext<'a, T, D, G> {
        SiblingsNext {
            next: Some(self.slot),
            tree: self.tree,
        }
    }

    pub fn children(&self) -> SiblingsNext<'a, T, D, G> {
        SiblingsNext {
            next: self.node().clink.first,
            // we know
            tree: self.tree,
        }
    }
}

// --------------------------------------------------------------------------------
// Automatic iterators

/// Return value `Traverse::next`
#[derive(Derivative)]
#[derivative(Debug(bound = "T: Debug"))]
pub enum TraverseItem<'a, T> {
    /// Sibling or rooted node
    Node(&'a Node<T>),
    Child(&'a Node<T>),
    // add parent index?
    EndChildren,
}

/// Depth-first search
#[derive(Derivative)]
#[derivative(Debug(bound = "T: Debug"))]
pub struct Traverse<'a, T, D = (), G: Gen = DefaultGen> {
    #[derivative(Debug = "ignore")]
    pub(crate) tree: &'a Tree<T, D, G>,
    /// We need `Vec` since the `Node` doesn't store parents
    pub(crate) states: Vec<TraverseState<'a, T, D, G>>,
}

/// Corresponds to a depth
#[derive(Derivative)]
#[derivative(Debug(bound = "T: Debug"))]
pub(crate) enum TraverseState<'a, T, D = (), G: Gen = DefaultGen> {
    /// The node and their children
    Parent(NodeRef<'a, T, D, G>),
    FirstChild(NodeRef<'a, T, D, G>),
    NonFirstChildren(SiblingsNext<'a, T, D, G>),
    ImplicitRootChildren(SiblingsNext<'a, T, D, G>),
}

impl<'a, T, D, G: Gen> Iterator for Traverse<'a, T, D, G> {
    type Item = TraverseItem<'a, T>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let last = self.states.last_mut()?;
            let res = match last {
                TraverseState::Parent(parent) => {
                    let parent = NodeRef {
                        tree: parent.tree,
                        slot: parent.slot,
                    };
                    let node = parent.node();

                    self.states.pop();
                    self.maybe_push_children(&node);

                    // the node
                    TraverseItem::Node(node)
                }
                TraverseState::FirstChild(child) => {
                    let node = self.tree.nodes.get_by_slot(child.slot).unwrap();

                    self.states.pop();
                    self.maybe_push_siblings(&node);
                    self.maybe_push_children(&node);

                    TraverseItem::Child(node)
                }
                TraverseState::NonFirstChildren(siblings) => match siblings.next() {
                    Some(next) => {
                        let cloned = NodeRef {
                            slot: next.slot,
                            tree: next.tree,
                        };
                        self.states.push(TraverseState::Parent(cloned));
                        continue;
                    }
                    None => {
                        self.states.pop();
                        TraverseItem::EndChildren
                    }
                },
                TraverseState::ImplicitRootChildren(siblings) => match siblings.next() {
                    Some(next) => {
                        let cloned = NodeRef {
                            slot: next.slot,
                            tree: next.tree,
                        };
                        self.states.push(TraverseState::Parent(cloned));
                        continue;
                    }
                    None => {
                        self.states.pop();
                        continue;
                    }
                },
            };
            break Some(res);
        }
    }
}

impl<'a, T, D, G: Gen> iter::FusedIterator for Traverse<'a, T, D, G> {}

impl<'a, T, D, G: Gen> Traverse<'a, T, D, G> {
    fn maybe_push_children(&mut self, node: &Node<T>) {
        if let Some(child) = node.clink.first {
            self.states.push(TraverseState::FirstChild(NodeRef {
                tree: self.tree,
                slot: child,
            }));
        }
    }

    fn maybe_push_siblings(&mut self, node: &Node<T>) {
        if let Some(sibling) = node.slink.next.clone() {
            let siblings = SiblingsNext {
                next: Some(sibling),
                tree: self.tree,
            };
            self.states.push(TraverseState::NonFirstChildren(siblings));
        }
    }
}
