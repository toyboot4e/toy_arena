/*!
Example code and documentation on safety

# Distinct type parameter `D`

Second type parameter of [`Arena<T, D, G>`] can be used for creating distinct arena types (`D=()` if not specified):

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
type of items, we could distinguish [`Index`] by the belonging container type.

# (UB) Arena cell

When we want to mutably borrow multiple items in the arena, we can use
[`Arena::get2_mut`], [`Arena::get3_mut`] or maybe [`ArenaCell`]:

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

// first mutable borrow:
let e1_mut = cell.get_mut(e1).unwrap();
e1_mut.hp += 10;

// second borrow of the same item results in panic at runtime:
// let e1_immutable = cell.get(e1).unwrap();

// second mutable borrow of another item is allowed:
let e2_mut = cell.get_mut(e2).unwrap();
e2_mut.hp += 10;

drop(cell);

assert_eq!(entities[e1], EntityModel { hp: 10 });
assert_eq!(entities[e2], EntityModel { hp: 11 });
```

They are UB, but they are better than index/slot-based access on user side.

# Note on safety

Some items in this crate are marked as `(UB)` ([undefined behavior]). It would be confirmed by
[miri] if you run your code with them.

[undefined behavior]: https://doc.rust-lang.org/reference/behavior-considered-undefined.html
[miri]: https://github.com/rust-lang/miri

[From the reference]:

> Breaking the pointer aliasing rules. &mut T and &T follow LLVM’s scoped noalias model, except if the &T contains an [`UnsafeCell<U>`].

[`UnsafeCell`]: https://doc.rust-lang.org/std/cell/struct.UnsafeCell.html
[From the reference]: https://doc.rust-lang.org/reference/behavior-considered-undefined.html

UB in Rust is like "unspecified behacior", and it doesn't mean runtime UB, but that the possibilitiy
is NOT zero.
*/

// for linking types in the docstring:
#[allow(unused)]
use super::*;
