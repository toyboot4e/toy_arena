/*!
Example documentation

# Distinct type parameter `D`

Second type parameter of [`Arena<T, D, G>`] is used for creating distinct arena types.

```
use toy_arena::{Arena, Index};

pub struct EntityModel {
    hp: u32,
}
pub struct Model {
    // arena of `EntityModel` with distinct type parameter `Model`
    entities: Arena<EntityModel, Self>,
}

pub struct EntityView {
    model: Index<EntityModel, Model>,
    name: String,
}
pub struct View {
    // arena of `EntityView` with distinct type parameter `View`
    entities: Arena<EntityView, Self>,
}
```

We don't need distinct type parameter in this example.. 😂 But if we have two arenas for the same
type of items, we could distinguish `Index` by the belonging container type.

# Arena cell

When we want to mutably borrow multiple items in the arena, we can use [`ArenaCell`].

```
use toy_arena::{Arena, Index};

#[derive(Debug, PartialEq)]
pub struct EntityModel {
    hp: u32,
}

let mut entities = Arena::<EntityModel>::new();
let e1 = entities.insert(EntityModel { hp: 0 });
let e2 = entities.insert(EntityModel { hp: 1 });

let cell = entities.cell();

let e1_mut = cell.get_mut(e1).unwrap();
e1_mut.hp += 10;

// panics on second borrow:
// let e1_immutable = cell.get(e1).unwrap();

let e2_mut = cell.get_mut(e2).unwrap();
e2_mut.hp += 10;

drop(cell);

assert_eq!(entities[e1], EntityModel { hp: 10 });
assert_eq!(entities[e2], EntityModel { hp: 11 });
```
*/

// for linking types in the docstring:
#[allow(unused)]
use super::*;
