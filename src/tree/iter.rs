/*!
Iterator types for [`Drt`]
*/

use std::{iter, marker::PhantomData};

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

// ----------------------------------------
// manual iteators, mainly for hinding `Slot` from user

/// Iterator that walks through siblings
pub struct SiblingsNext<'a, T, D, G: Gen> {
    pub(crate) next: Option<Slot>,
    pub(crate) tree: &'a Tree<T, D, G>,
}

impl<'a, T, D, G: Gen> Iterator for SiblingsNext<'a, T, D, G> {
    type Item = NodeRef<'a, T, D, G>;
    fn next(&mut self) -> Option<Self::Item> {
        let slot = self.next?;
        let node = self.tree.nodes.get_by_slot(slot).unwrap();
        self.next = node.slink.next;
        Some(NodeRef {
            slot,
            tree: self.tree,
        })
    }
}

/// Reference to a node and their children
pub struct NodeRef<'a, T, D, G: Gen> {
    pub(crate) slot: Slot,
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

    pub fn siblings(&self) -> impl Iterator<Item = NodeRef<'a, T, D, G>> {
        SiblingsNext {
            next: self.node().slink.next,
            tree: self.tree,
        }
    }

    /// This node and sublings
    pub fn preorder(&self) -> impl Iterator<Item = NodeRef<'a, T, D, G>> {
        SiblingsNext {
            next: Some(self.slot),
            tree: self.tree,
        }
    }

    pub fn children(&self) -> impl Iterator<Item = NodeRef<'a, T, D, G>> {
        SiblingsNext {
            next: {
                let parent = self.tree.nodes.get_by_slot(self.slot).unwrap();
                parent.clink.first
            },
            // we know
            tree: self.tree,
        }
    }
}

// // ----------------------------------------
// // automatic iterators
//
// pub enum TraverseItem<'a, T> {
//     SameDepth(&'a T),
//     Nest(&'a T),
//     Pop,
// }
//
// /// Depth-first iterator of node
// pub struct Traverse<'a, T, D, G: Gen> {
//     tree: &'a Tree<T, D, G>,
//     /// We need `Vec` since the `Node` doesn't store parents
//     states: Vec<TraverseState>,
//     _ty1: PhantomData<fn() -> T>,
//     _ty2: PhantomData<fn() -> D>,
//     _ty3: PhantomData<fn() -> G>,
// }
//
// /// Corresponds to a depth
// struct TraverseState {
//     // node: Slot,
//     /// Next node in this depth
//     next: Option<Slot>,
// }
//
// impl<'a, T, D, G: Gen> Traverse<'a, T, D, G> {
//     pub(crate) fn new(tree: &'a Tree<T, D, G>, root: Slot) -> Self {
//         Self {
//             tree,
//             states: vec![TraverseState {
//                 // node: root,
//                 next: Some(root),
//             }],
//             _ty1: PhantomData,
//             _ty2: PhantomData,
//             _ty3: PhantomData,
//         }
//     }
// }
//
// impl<'a, T, D, G: Gen> Iterator for Traverse<'a, T, D, G> {
//     type Item = TraverseItem<'a, T>;
//
//     fn next(&mut self) -> Option<Self::Item> {
//         // find state with some next node
//         let state = loop {
//             let state = self.states.last_mut()?;
//             if !state.next.is_some() {
//                 // pop
//                 self.states.pop();
//                 // continue;
//                 return Some(TraverseItem::Pop);
//             }
//             break state;
//         };
//
//         let slot = state.next.unwrap();
//         let entry = &self.tree.nodes.entries[slot.raw as usize];
//         let node = entry.data.as_ref().unwrap();
//
//         state.next = node.next;
//
//         if let Some(child) = node.first_child {
//             // nest
//             self.states.push(TraverseState { next: Some(child) });
//             Some(self.next().unwrap())
//         } else {
//             // same depth
//             Some(TraverseItem::SameDepth(node.data()))
//         }
//     }
// }
//
// impl<'a, T, D, G: Gen> iter::FusedIterator for Traverse<'a, T, D, G> {}
