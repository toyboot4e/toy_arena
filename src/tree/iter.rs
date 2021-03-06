/*!
Iterators types for the [`Tree`]
*/

use std::iter;

use derivative::Derivative;

use super::*;

/// Debug-only validation
#[cfg(debug_assertions)]
macro_rules! validate_slot {
    ($tree:expr, $slot:expr) => {{
        let entry = $tree.nodes.entries.get($slot.to_usize()).unwrap();
        let node = entry.data.as_ref().unwrap();
        if node.link.has_any_child() {
            assert_ne!(
                node.link.first_child(),
                node.link.last_child(),
                "bug: first/last child must not be the same node"
            );
        }
    }};
}

/// Debug-only validation
#[cfg(not(debug_assertions))]
macro_rules! validate_slot {
    ($tree:expr, $slot:expr) => {};
}

/// Binding of existing node and their children
#[derive(Derivative)]
#[derivative(Debug(bound = "T: Debug"))]
pub struct NodeRef<'a, T, D = (), G: Gen = DefaultGen> {
    slot: Slot,
    #[derivative(Debug = "ignore")]
    tree: &'a Tree<T, D, G>,
}

impl<'a, T, D, G: Gen> NodeRef<'a, T, D, G> {
    pub fn new(slot: Slot, tree: &'a Tree<T, D, G>) -> Self {
        validate_slot!(tree, slot);
        Self { slot, tree }
    }

    pub fn id(&self) -> NodeId<T, D, G> {
        // we now the targetting entry exists
        self.tree.nodes.upgrade(self.slot).unwrap()
    }

    pub(crate) fn node(&self) -> &'a Node<T, D, G> {
        self.tree.nodes.get_by_slot(self.slot).unwrap()
    }

    pub fn data(&self) -> &'a T {
        self.node().data()
    }

    /// Nodes after this node
    pub fn siblings(&self) -> SiblingsNext<'a, T, D, G> {
        SiblingsNext {
            next: self.node().link.next_sibling(),
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
            next: self.node().link.first_child(),
            // we know
            tree: self.tree,
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
pub struct SiblingsNext<'a, T, D = (), G: Gen = DefaultGen> {
    /// Can be `None`
    pub(crate) next: Option<Slot>,
    #[derivative(Debug = "ignore")]
    pub(crate) tree: &'a Tree<T, D, G>,
}

impl<'a, T, D, G: Gen> Iterator for SiblingsNext<'a, T, D, G> {
    type Item = NodeRef<'a, T, D, G>;
    fn next(&mut self) -> Option<Self::Item> {
        let next = self.next?;
        let next_node = self
            .tree
            .nodes
            .get_by_slot(next)
            .expect("bug: invalid `next`");
        self.next = next_node.link.next_sibling();
        Some(NodeRef::new(next, self.tree))
    }
}

// --------------------------------------------------------------------------------
// Automatic iterators

/// Return value of [`Traverse::next`]
#[derive(Derivative)]
#[derivative(Debug(bound = "T: Debug"))]
pub enum TraverseItem<'a, T, D, G: Gen> {
    /// Sibling or rooted node
    Node(&'a Node<T, D, G>),
    Child(&'a Node<T, D, G>),
    // add parent index?
    EndChildren,
}

/// Generic traersing utility
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
    MultileRootNodes(SiblingsNext<'a, T, D, G>),
}

impl<'a, T, D, G: Gen> Iterator for Traverse<'a, T, D, G> {
    type Item = TraverseItem<'a, T, D, G>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let last = self.states.last_mut()?;
            let res = match last {
                TraverseState::Parent(parent) => {
                    let parent = NodeRef::new(parent.slot, parent.tree);
                    let node = parent.node();

                    self.states.pop();
                    self.maybe_push_children(&node);
                    TraverseItem::Node(node)
                }
                TraverseState::FirstChild(child) => {
                    let node = self.tree.nodes.get_by_slot(child.slot).unwrap();

                    self.states.pop();
                    // siblings of the child
                    self.push_siblings_of_first_child(&node);
                    // children of the child
                    self.maybe_push_children(&node);
                    TraverseItem::Child(node)
                }
                TraverseState::NonFirstChildren(siblings) => match siblings.next() {
                    Some(next) => {
                        let cloned = NodeRef::new(next.slot, next.tree);
                        self.states.push(TraverseState::Parent(cloned));
                        continue;
                    }
                    None => {
                        self.states.pop();
                        TraverseItem::EndChildren
                    }
                },
                TraverseState::MultileRootNodes(siblings) => match siblings.next() {
                    Some(next) => {
                        let cloned = NodeRef::new(next.slot, next.tree);
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
    fn maybe_push_children(&mut self, node: &Node<T, D, G>) {
        if let Some(child) = node.link.first_child() {
            self.states.push(TraverseState::FirstChild(NodeRef {
                tree: self.tree,
                slot: child,
            }));
        } else {
            debug_assert!(node.link.last_child().is_none());
        }
    }

    fn push_siblings_of_first_child(&mut self, node: &Node<T, D, G>) {
        let siblings = SiblingsNext {
            next: node.link.next_sibling(),
            tree: self.tree,
        };
        self.states.push(TraverseState::NonFirstChildren(siblings));
    }
}
