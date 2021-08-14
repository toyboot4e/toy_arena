/*!
Iterator types for [`Drt`]
*/

use std::marker::PhantomData;

use super::*;

pub struct Children<'a, T, D, G: Gen> {
    tree: &'a Drt<T, D, G>,
    current: Slot,
    _ty1: PhantomData<fn() -> T>,
    _ty2: PhantomData<fn() -> D>,
    _ty3: PhantomData<fn() -> G>,
}

impl<'a, T, D, G: Gen> Iterator for Children<'a, T, D, G> {
    type Item = &'a Node<T>;
    fn next(&mut self) -> Option<Self::Item> {
        todo!()
    }
}
