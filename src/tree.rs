/*!
Tree layered on top of the generational arena

# Similar crates
* [indextree](https://docs.rs/indextree/latest/indextree/)
* [ego_tree](https://docs.rs/ego-tree/latest/ego_tree/)

WARNING: This module **definitely** needs more tests!
*/

// TODO: deep clone
// TODO: directon type parameter
// TODO: impl double-ended iterator

pub mod iter;
pub mod iter_mut;

// The `tree!` macro is defined in this module but exported at the crate root (unfortunatelly)
#[doc(inline)]
pub use crate::tree;

/// [`Tree`] without generations
pub type VecTree<T, D> = Tree<T, D, ()>;

impl Gen for () {
    fn default_gen() -> Self {
        ()
    }
    fn next(&mut self) -> Self {
        ()
    }
}

// just for doc link
#[allow(unused)]
use crate::Index;

#[cfg(test)]
mod test;

pub use iter::TraverseItem;

use std::{fmt::Debug, hash::Hash, ops};

use derivative::Derivative;

use crate::{DefaultGen, Gen, Slot};

/// Tree node index with parenting API
pub type NodeId<T, D = (), G = DefaultGen> = crate::Index<Node<T>, D, G>;

type NodeArena<T, D, G> = crate::Arena<Node<T>, D, G>;

/**
Tree layered on top of [`Arena`](crate::Arena). See [`NodeId`] for parenting methods.

# Implmentation note

[`Tree`] doesn't have root node and the API is a bit similar to [`Arena`](crate::Arena). But wo
could use an explicit root node, where `parent` of `Node` is always there (if it's not the root node).
*/
#[derive(Derivative)]
#[derivative(
    Debug(bound = "T: Debug"),
    PartialEq(bound = "T: PartialEq"),
    Eq(bound = "T: PartialEq"),
    Hash(bound = "T: Hash")
)]
pub struct Tree<T, D = (), G: Gen = DefaultGen> {
    nodes: NodeArena<T, D, G>,
    /// Corresponds to the implicit root
    root: ChildLink,
}

/// Opaque tree node index
#[derive(Derivative)]
#[derivative(
    Debug(bound = "T: Debug"),
    PartialEq(bound = "T: PartialEq"),
    Eq(bound = "T: Eq"),
    Hash(bound = "T: Hash")
)]
pub struct Node<T> {
    token: T,
    slink: SiblingsLink,
    clink: ChildLink,
    /// `None` refers to the implicit root node. Parent handle is needed when removing the node
    /// later.
    parent: Option<Slot>,
}

/// Doubly linked list indices for siblings
#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
struct SiblingsLink {
    // TODO use a nonmax type for slots
    /// Next sibling
    next: Option<Slot>,
    /// Previous sibling
    prev: Option<Slot>,
}

/// Indices for referring to children. NOTE: The first and last fields must not overlap.
#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
struct ChildLink {
    first: Option<Slot>,
    last: Option<Slot>,
}

impl SiblingsLink {
    /// Fixes siblings link on removal. NOTE: It doesn't mutate the removed node's sibling links!
    pub fn on_remove<T, D, G: Gen>(self, tree: &mut Tree<T, D, G>) {
        if let Some(prev) = self.prev {
            let prev = tree.nodes.get_mut_by_slot(prev).unwrap();
            prev.slink.next = self.next;
        }
        if let Some(next) = self.next {
            let next = tree.nodes.get_mut_by_slot(next).unwrap();
            next.slink.prev = self.prev;
        }
    }
}

impl ChildLink {
    pub fn has_any(&self) -> bool {
        self.first.is_some() || self.last.is_some()
    }

    /// Fixes parent/child link on leaf node removal
    pub fn on_remove_leaf<T, D, G: Gen>(self, child_slot: Slot, tree: &mut Tree<T, D, G>) {
        // NOTE: The first and last fields must not overlap.
        debug_assert!(self.first != self.last);

        // Fix the parent-child link
        if let Some(first_slot) = self.first {
            if first_slot == child_slot {
                let first_node = tree.node_by_slot(first_slot).unwrap();
                debug_assert!(first_node.slink.prev.is_none());

                if let Some(second_slot) = first_node.slink.next {
                    let second_node = tree.node_mut_by_slot(second_slot).unwrap();
                    debug_assert!(second_node.slink.prev == Some(first_slot));
                    second_node.slink.prev = None;
                }

                return;
            }
        }

        if let Some(last_slot) = self.last {
            if last_slot == child_slot {
                let last_node = tree.node_by_slot(last_slot).unwrap();
                debug_assert!(last_node.slink.next.is_none());

                if let Some(before_last_slot) = last_node.slink.prev {
                    let before_last_node = tree.node_mut_by_slot(before_last_slot).unwrap();
                    debug_assert!(before_last_node.slink.next == Some(last_slot));
                    before_last_node.slink.next = None;
                }

                return;
            }
        }
    }
}

pub(crate) fn on_insert_to_implicit_root<T, D, G: Gen>(
    id: NodeId<T, D, G>,
    tree: &mut Tree<T, D, G>,
) {
    // linking
    if tree.root.first.is_none() {
        tree.root.first = Some(id.slot());
    } else {
        // NOTE: The first and last fields must not overlap.
        debug_assert_ne!(tree.root.first, tree.root.last);

        if let Some(last_slot) = tree.root.last.or(tree.root.first) {
            let last_node = tree.node_mut_by_slot(last_slot).unwrap();
            debug_assert!(last_node.slink.next.is_none());
            last_node.slink.next = Some(id.slot);
            let node = tree.node_mut_by_slot(id.slot).unwrap();
            node.slink.prev = Some(last_slot);
        }

        tree.root.last = Some(id.slot());
    }
}

impl<T> Node<T> {
    pub fn parent_slot(&self) -> Option<Slot> {
        self.parent
    }

    pub fn next_sibling_slot(self) -> Option<Slot> {
        self.slink.next
    }

    pub fn prev_sibling_slot(self) -> Option<Slot> {
        self.slink.prev
    }

    pub fn first_child_slot(self) -> Option<Slot> {
        self.clink.first
    }

    pub fn last_child_slot(self) -> Option<Slot> {
        self.clink.last
    }
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
            root: Default::default(),
        }
    }

    pub fn contains(&self, index: NodeId<T, D, G>) -> bool {
        self.nodes.contains(index)
    }

    /// Number of items in this tree
    pub fn len(&self) -> usize {
        self.nodes.len()
    }

    /// The capacity of the backgin vec
    pub fn capacity(&self) -> usize {
        self.nodes.capacity()
    }
}

/// # ----- Node accessors -----
impl<T, D, G: Gen> Tree<T, D, G> {
    /// Returns a reference to the node
    pub fn node(&self, id: NodeId<T, D, G>) -> Option<&Node<T>> {
        self.nodes.get(id)
    }

    /// Returns a mutable reference to the node
    pub fn node_mut(&mut self, id: NodeId<T, D, G>) -> Option<&mut Node<T>> {
        self.nodes.get_mut(id)
    }

    /// Returns a reference to the node
    pub fn node_by_slot(&self, slot: Slot) -> Option<&Node<T>> {
        self.nodes.get_by_slot(slot)
    }

    /// Returns a mutable reference to the node
    pub fn node_mut_by_slot(&mut self, slot: Slot) -> Option<&mut Node<T>> {
        self.nodes.get_mut_by_slot(slot)
    }

    /// Returns reference to the data in a node
    pub fn data(&self, id: NodeId<T, D, G>) -> Option<&T> {
        self.nodes.get(id).map(Node::data)
    }

    /// Returns mutable reference to the data in a node
    pub fn data_mut(&mut self, id: NodeId<T, D, G>) -> Option<&mut T> {
        self.nodes.get_mut(id).map(Node::data_mut)
    }

    /// Appends a new data to the implicit root node
    pub fn insert(&mut self, token: T) -> NodeId<T, D, G> {
        let node = Node::root(token);
        let id = self.nodes.insert(node);

        self::on_insert_to_implicit_root(id, self);
        id
    }

    /// Mutablly binds a node. Most mutation should happen here
    pub fn bind<'a>(&'a mut self, id: NodeId<T, D, G>) -> Option<iter_mut::NodeMut<'a, T, D, G>> {
        if !self.nodes.contains(id) {
            None
        } else {
            Some(iter_mut::NodeMut::bind(self, id.slot))
        }
    }
}

/// # ----- Traversal terators -----
impl<T, D, G: Gen> Tree<T, D, G> {
    /// Sub tree rooted at the node (depth-first, preorder)
    pub fn subtree(&self, id: NodeId<T, D, G>) -> iter::Traverse<T, D, G> {
        let states = vec![iter::TraverseState::Parent(iter::NodeRef::new(
            id.slot, self,
        ))];
        iter::Traverse { tree: self, states }
    }

    /// Sub trees rooted at this node and the siblings (depth-first, preorder)
    pub fn traverse(&self, id: NodeId<T, D, G>) -> iter::Traverse<T, D, G> {
        let states = vec![iter::TraverseState::MultileRootNodes(iter::SiblingsNext {
            next: Some(id.slot),
            tree: self,
        })];
        iter::Traverse { tree: self, states }
    }

    /// Sub trees at the root
    pub fn traverse_root_nodes(&self) -> iter::Traverse<T, D, G> {
        let mut states = vec![];
        states.push(iter::TraverseState::MultileRootNodes(iter::SiblingsNext {
            next: self.root.first,
            tree: self,
        }));
        iter::Traverse { tree: self, states }
    }
}

/// # ----- Flat, non-recursive iterators -----
impl<T, D, G: Gen> Tree<T, D, G> {
    /// Sub trees rooted at siblings after this node (depth-first, preorder)
    pub fn siblings(&self, id: NodeId<T, D, G>) -> iter::Traverse<T, D, G> {
        let states = vec![iter::TraverseState::MultileRootNodes(iter::SiblingsNext {
            next: self.node(id).and_then(|n| n.slink.next),
            tree: self,
        })];
        iter::Traverse { tree: self, states }
    }

    /// Children (depth-first, preorder)
    pub fn children(&mut self, id: NodeId<T, D, G>) -> iter::SiblingsNext<T, D, G> {
        let first = self.node(id).and_then(|node| node.clink.first);
        iter::SiblingsNext {
            next: first,
            tree: self,
        }
    }

    /// Returns iterator of child node bindings
    pub fn children_mut(&mut self, id: NodeId<T, D, G>) -> iter_mut::SiblingsMutNext<T, D, G> {
        let first = self.node(id).and_then(|node| node.clink.first);
        let bind = iter_mut::TreeBind::new(self);
        iter_mut::SiblingsMutNext { bind, next: first }
    }

    pub fn root_nodes(&self) -> iter::SiblingsNext<T, D, G> {
        iter::SiblingsNext {
            next: self.root.first,
            tree: self,
        }
    }

    /// Returns iterator of child node bindings
    pub fn root_nodes_mut(&mut self) -> iter_mut::SiblingsMutNext<T, D, G> {
        let first = self.root.first;
        let bind = iter_mut::TreeBind::new(self);
        iter_mut::SiblingsMutNext { bind, next: first }
    }
}

impl<T, D, G: Gen> ops::Index<NodeId<T, D, G>> for Tree<T, D, G> {
    type Output = Node<T>;
    fn index(&self, id: NodeId<T, D, G>) -> &Self::Output {
        self.node(id).unwrap()
    }
}

impl<T, D, G: Gen> ops::IndexMut<NodeId<T, D, G>> for Tree<T, D, G> {
    fn index_mut(&mut self, id: NodeId<T, D, G>) -> &mut Self::Output {
        self.node_mut(id).unwrap()
    }
}

impl<T> Node<T> {
    fn from_parent(token: T, parent: Slot) -> Self {
        Self {
            token,
            clink: Default::default(),
            slink: Default::default(),
            parent: Some(parent),
        }
    }

    fn root(token: T) -> Self {
        Self {
            token,
            clink: Default::default(),
            slink: Default::default(),
            parent: None,
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

/// # ---- Tree node impls -----
impl<T, D, G: Gen> NodeId<T, D, G> {
    /// Attaches a child to the node
    pub fn attach(self, child: T, tree: &mut Tree<T, D, G>) -> Option<NodeId<T, D, G>> {
        if !tree.contains(self) {
            return None;
        };

        let child_node = Node::from_parent(child, self.slot);
        let child_id = tree.nodes.insert(child_node);
        let child_slot = child_id.slot();

        // siblings link
        let self_node = tree.node_mut(self).unwrap();
        if let Some(last_slot) = self_node.clink.last.or(self_node.clink.first) {
            let (last_node, child_node) =
                tree.nodes.get2_mut_by_slot(last_slot, child_slot).unwrap();
            debug_assert!(last_node.slink.next.is_none());
            last_node.slink.next = Some(child_slot);
            child_node.slink.prev = Some(last_slot);
        }

        // parent -> child link
        let self_node = tree.node_mut(self).unwrap();

        if self_node.clink.first.is_none() {
            debug_assert!(self_node.clink.last.is_none());
            self_node.clink.first = Some(child_slot);
        } else {
            self_node.clink.last = Some(child_slot);
        }

        Some(child_id)
    }
}

/**
Creates a [`Tree`] with given hierarchy of value

```
use toy_arena::{tree, tree::Tree};

let tree: Tree<usize> = tree! {
    0,
    1, {
        10,
        11, {
            100,
            101,
        },
        12,
    },
};
```
*/
#[macro_export]
macro_rules! tree {
    ($($x:tt),* $(,)?) => {{
        let mut tree = $crate::tree::Tree::new();
        tree!(@ tree, $($x),*);
        tree
    }};

    // base pattern
    (@ $tree:expr $(,)?) => {
    };

    // ---------- ROOT ----------
    // We don't haveparent node yet

    // NOTE: Be sure to match `{ .. } ` in early rules and `tt` in later rules

    // root
    (@ $tree:expr, $data:expr, { $($cs:tt),* $(,)? } $(,)?) => {
        {
            let parent = $tree.insert($data);
            tree!(@@ $tree, parent, $($cs),*);
        }
    };

    // root + rest
    (@ $tree:expr, $data:expr, { $($cs:tt),* $(,)? }, $($rest:tt),* $(,)?) => {
        {
            let parent = $tree.insert($data);
            tree!(@@ $tree, parent, $($cs),*);
        }
        tree!(@ $tree, $($rest),*);
    };

    // leaf
    (@ $tree:expr, $l:expr $(,)?) => {
        $tree.insert($l);
    };

    // leaf + rest
    (@ $tree:expr, $l:expr, $($rest:tt),+ $(,)?) => {
        $tree.insert($l);
        tree!(@ $tree, $($rest),*);
    };

    // ---------- PARENT ----------
    // We have parent node index `$p`

    // base
    (@@ $tree:expr, $p:expr $(,)?) => {};

    // root
    (@@ $tree:expr, $p:expr, $c:expr, { $($cs:tt),* $(,)? } $(,)?) => {
        {
            let parent = $c.attach($c, &mut $tree).unwrap();
            tree!(@@ $tree, parent, $($cs),*);
        }
    };

    // root + rest
    (@@ $tree:expr, $p:expr, $c:expr, { $($cs:tt),* $(,)? }, $($rest:tt)* $(,)?) => {
        {
            let parent = $p.attach($c, &mut $tree).unwrap();
            tree!(@@ $tree, parent, $($cs),*);
        }
        tree!(@@ $tree, $p, $($rest),*);
    };

    // leaf
    (@@ $tree:expr, $p:expr, $l:expr $(,)?) => {
        $p.attach($l, &mut $tree).unwrap();
    };

    // leaf + rest
    (@@ $tree:expr, $p:expr, $l:expr, $($rest:tt),* $(,)?) => {
        $p.attach($l, &mut $tree).unwrap();
        tree!(@@ $tree, $p, $($rest),*);
    };
}
