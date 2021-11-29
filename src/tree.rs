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

mod link;
use link::Link;

#[cfg(test)]
mod test;

// The `tree!` macro is defined in this module but exported at the crate root (unfortunatelly)
#[doc(inline)]
pub use crate::tree;

pub use iter::TraverseItem;

// just for doc link
#[allow(unused)]
use crate::Index;

use std::{fmt::Debug, hash::Hash, marker::PhantomData, ops};

use derivative::Derivative;

#[cfg(feature = "igri")]
use igri::Inspect;

use crate::{DefaultGen, Gen, Slot};

/// Tree node index with parenting API
pub type NodeId<T, D = (), G = DefaultGen> = crate::Index<Node<T, D, G>, D, G>;

type NodeArena<T, D, G> = crate::Arena<Node<T, D, G>, D, G>;

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
#[cfg_attr(
    feature = "igri",
    derive(Inspect),
    inspect(with = "inspect_tree", bounds = "T: Inspect")
)]
pub struct Tree<T, D = (), G: Gen = DefaultGen> {
    nodes: NodeArena<T, D, G>,
    /// Corresponds to the implicit root
    root: Link<Slot>,
}

impl<T, D, G: Gen> link::Id<Slot> for NodeId<T, D, G> {
    fn slot(&self) -> Slot {
        self.slot
    }
}

impl<T, D, G: Gen> link::Tree for Tree<T, D, G> {
    type Slot = Slot;
    type Id = NodeId<T, D, G>;

    fn root_mut(&mut self) -> &mut Link<Self::Slot> {
        &mut self.root
    }

    fn link_mut_by_slot(&mut self, slot: Self::Slot) -> Option<&mut Link<Self::Slot>> {
        self.nodes.get_mut_by_slot(slot).map(|n| &mut n.link)
    }

    fn link2_mut_by_slot(
        &mut self,
        s0: Self::Slot,
        s1: Self::Slot,
    ) -> Option<(&mut Link<Self::Slot>, &mut Link<Self::Slot>)> {
        self.nodes
            .get2_mut_by_slot(s0, s1)
            .map(|(n0, n1)| (&mut n0.link, &mut n1.link))
    }

    fn link_mut_by_id(&mut self, id: Self::Id) -> Option<&mut Link<Self::Slot>> {
        self.nodes.get_mut(id).map(|n| &mut n.link)
    }
}

#[cfg(feature = "igri")]
fn inspect_tree<'a, T, D, G: Gen>(tree: &'a mut Tree<T, D, G>, ui: &igri::imgui::Ui, label: &str)
where
    T: igri::Inspect,
{
    crate::inspect_arena(&mut tree.nodes, ui, label);
}

/// Opaque tree node index
#[derive(Derivative)]
#[derivative(
    Debug(bound = "T: Debug"),
    PartialEq(bound = "T: PartialEq"),
    Eq(bound = "T: Eq"),
    Hash(bound = "T: Hash")
)]
#[cfg_attr(
    feature = "igri",
    derive(Inspect),
    inspect(with = "inspect_node", bounds = "T: Inspect")
)]
pub struct Node<T, D = (), G: Gen = DefaultGen> {
    token: T,
    link: Link<Slot>,
    _d: PhantomData<fn() -> D>,
    _g: PhantomData<fn() -> G>,
}

#[cfg(feature = "igri")]
fn inspect_node<'a, T, D, G: Gen>(node: &mut Node<T, D, G>, ui: &igri::imgui::Ui, label: &str)
where
    T: igri::Inspect,
{
    node.token.inspect(ui, label);
}

impl<T, D, G: Gen> Node<T, D, G> {
    fn from_parent(token: T, parent: Slot) -> Self {
        Self {
            token,
            link: Link::with_parent(parent),
            _d: PhantomData,
            _g: PhantomData,
        }
    }

    fn root(token: T) -> Self {
        Self {
            token,
            link: Link::default(),
            _d: PhantomData,
            _g: PhantomData,
        }
    }

    pub fn parent_slot(&self) -> Option<Slot> {
        self.link.parent()
    }

    pub fn next_sibling_slot(self) -> Option<Slot> {
        self.link.next_sibling()
    }

    pub fn prev_sibling_slot(self) -> Option<Slot> {
        self.link.prev_sibling()
    }

    pub fn first_child_slot(self) -> Option<Slot> {
        self.link.first_child()
    }

    pub fn last_child_slot(self) -> Option<Slot> {
        self.link.last_child()
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
            root: Link::default(),
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
    pub fn node(&self, id: NodeId<T, D, G>) -> Option<&Node<T, D, G>> {
        self.nodes.get(id)
    }

    /// Returns a mutable reference to the node
    pub fn node_mut(&mut self, id: NodeId<T, D, G>) -> Option<&mut Node<T, D, G>> {
        self.nodes.get_mut(id)
    }

    /// Returns a reference to the node
    pub fn node_by_slot(&self, slot: Slot) -> Option<&Node<T, D, G>> {
        self.nodes.get_by_slot(slot)
    }

    /// Returns a mutable reference to the node
    pub fn node_mut_by_slot(&mut self, slot: Slot) -> Option<&mut Node<T, D, G>> {
        self.nodes.get_mut_by_slot(slot)
    }

    /// Returns reference to the data in the node
    pub fn data(&self, id: NodeId<T, D, G>) -> Option<&T> {
        self.nodes.get(id).map(Node::data)
    }

    /// Returns mutable reference to the data in the node
    pub fn data_mut(&mut self, id: NodeId<T, D, G>) -> Option<&mut T> {
        self.nodes.get_mut(id).map(Node::data_mut)
    }

    /// Appends a new data to the implicit root node
    pub fn insert(&mut self, token: T) -> NodeId<T, D, G> {
        let node = Node::root(token);
        let id = self.nodes.insert(node);

        link::fix_root_on_insert(self, id);
        id
    }
}

/// # ----- Binding -----
impl<T, D, G: Gen> Tree<T, D, G> {
    /// Binds a node mutably. Most mutation should happen here!
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
            next: self.root.first_child(),
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
            next: self.node(id).and_then(|n| n.link.next_sibling()),
            tree: self,
        })];
        iter::Traverse { tree: self, states }
    }

    /// Children (depth-first, preorder)
    pub fn children(&mut self, id: NodeId<T, D, G>) -> iter::SiblingsNext<T, D, G> {
        let first = self.node(id).and_then(|node| node.link.first_child());
        iter::SiblingsNext {
            next: first,
            tree: self,
        }
    }

    /// Returns iterator of child node bindings
    pub fn children_mut(&mut self, id: NodeId<T, D, G>) -> iter_mut::SiblingsMutNext<T, D, G> {
        let first = self.node(id).and_then(|node| node.link.first_child());
        let bind = iter_mut::TreeBind::new(self);
        iter_mut::SiblingsMutNext { bind, next: first }
    }

    pub fn root_nodes(&self) -> iter::SiblingsNext<T, D, G> {
        iter::SiblingsNext {
            next: self.root.first_child(),
            tree: self,
        }
    }

    /// Returns iterator of child node bindings
    pub fn root_nodes_mut(&mut self) -> iter_mut::SiblingsMutNext<T, D, G> {
        let first = self.root.first_child();
        let bind = iter_mut::TreeBind::new(self);
        iter_mut::SiblingsMutNext { bind, next: first }
    }
}

impl<T, D, G: Gen> ops::Index<NodeId<T, D, G>> for Tree<T, D, G> {
    type Output = Node<T, D, G>;
    fn index(&self, id: NodeId<T, D, G>) -> &Self::Output {
        self.node(id).unwrap()
    }
}

impl<T, D, G: Gen> ops::IndexMut<NodeId<T, D, G>> for Tree<T, D, G> {
    fn index_mut(&mut self, id: NodeId<T, D, G>) -> &mut Self::Output {
        self.node_mut(id).unwrap()
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
        link::fix_on_attach_child(self, child_id, tree);
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
        $crate::tree::tree!(@ tree, $($x),*);
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
            $crate::tree::tree!(@@ $tree, parent, $($cs),*);
        }
    };

    // root + rest
    (@ $tree:expr, $data:expr, { $($cs:tt),* $(,)? }, $($rest:tt),* $(,)?) => {
        {
            let parent = $tree.insert($data);
            $crate::tree::tree!(@@ $tree, parent, $($cs),*);
        }
        $crate::tree::tree!(@ $tree, $($rest),*);
    };

    // leaf
    (@ $tree:expr, $l:expr $(,)?) => {
        $tree.insert($l);
    };

    // leaf + rest
    (@ $tree:expr, $l:expr, $($rest:tt),+ $(,)?) => {
        $tree.insert($l);
        $crate::tree::tree!(@ $tree, $($rest),*);
    };

    // ---------- PARENT ----------
    // We have parent node index `$p`

    // base
    (@@ $tree:expr, $p:expr $(,)?) => {};

    // root
    (@@ $tree:expr, $p:expr, $c:expr, { $($cs:tt),* $(,)? } $(,)?) => {
        {
            let parent = $c.attach($c, &mut $tree).unwrap();
            $crate::tree::tree!(@@ $tree, parent, $($cs),*);
        }
    };

    // root + rest
    (@@ $tree:expr, $p:expr, $c:expr, { $($cs:tt),* $(,)? }, $($rest:tt)* $(,)?) => {
        {
            let parent = $p.attach($c, &mut $tree).unwrap();
            $crate::tree::tree!(@@ $tree, parent, $($cs),*);
        }
        $crate::tree::tree!(@@ $tree, $p, $($rest),*);
    };

    // leaf
    (@@ $tree:expr, $p:expr, $l:expr $(,)?) => {
        $p.attach($l, &mut $tree).unwrap();
    };

    // leaf + rest
    (@@ $tree:expr, $p:expr, $l:expr, $($rest:tt),* $(,)?) => {
        $p.attach($l, &mut $tree).unwrap();
        $crate::tree::tree!(@@ $tree, $p, $($rest),*);
    };
}
