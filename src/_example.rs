/*!
Example code and documentation

# Basic usage

[`Arena<T>`] is basically a vec, but with fixed item positions and generation value per slot:

```
use toy_arena::{arena, Arena, Index};

let mut xs = Arena::<usize>::with_capacity(1);

// Arena returns item index on insertion:
let x0: Index<usize> = xs.insert(0);

// Retrieve the item with index:
assert_eq!(xs.get(x0), Some(&0));
assert_eq!(xs.get_mut(x0), Some(&mut 0));
assert_eq!(xs[x0], 0);

// If we `remove` or `invaldate` the entry, the old index no longer works:
xs.invalidate(x0);
assert_eq!(xs.get(x0), None);

// Insert new item (to the same slot since the capacity is 1):
let x0_new = xs.insert(10);
assert_eq!(xs.get(x0_new), Some(&10));

// The old index doesn't work..
assert_eq!(xs.get(x0), None);
// ..because the generation of the index doesn't match to the
// generation of the current entry.
```

See [`Arena`] for more information.

# Tip: mutable binding iterators

For example, when you want to remove some kind of items from [`Arena`], use [`Arena::bindings`]:

```
use toy_arena::{arena, Arena};
let mut arena: Arena<usize> = arena![0, 1, 2, 3, 4, 5];

let mut drain = Vec::new();
for entry in arena.bindings() {
    if entry.get() % 2 == 0 {
        let data = entry.remove();
        drain.push(data);
    }
}

assert!(drain.iter().all(|x| x % 2 == 0));
assert!(arena.items().all(|x| x % 2 == 1));
```

We can also do [`Slot`]-based iteration, but it's not so handy, so I don't recommend.

# Feature: Distinct type parameter `D`

[`Arena<T, D, G>`] accetpts type parameter `D` for creating distinct arena types:

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
type of items, we could distinguish item [`Index`] by the belonging container type.
*/

// for linking types in the docstring:
#[allow(unused)]
use super::*;
