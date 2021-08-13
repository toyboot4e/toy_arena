use super::*;
use std::mem;

#[test]
fn test_size() {
    // `Index` is 8 bytes long by default
    assert_eq!(mem::size_of::<Index<()>>(), mem::size_of::<u32>() * 2);

    // the nonzero type reduces the optional index size
    assert_eq!(
        mem::size_of::<Option<Index<()>>>(),
        mem::size_of::<Index<()>>()
    );

    // unfortunatelly, entry is a bit too long with occupied tag
    assert_eq!(mem::size_of::<Entry<u32>>(), 12);
}

#[test]
fn insert_remove_invalidate_replace() {
    #[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
    pub struct Entity {
        pub hp: usize,
    }

    let mut entities = Arena::<Entity>::with_capacity(2);
    assert_eq!(entities.len(), 0);
    assert_eq!(entities.capacity(), 2);

    let index0: Index<Entity> = entities.insert(Entity { hp: 0 });
    assert_eq!(index0.slot(), unsafe { Slot::from_raw(0) });
    assert_eq!(entities.len(), 1);
    assert_eq!(entities.capacity(), 2);

    let index1: Index<Entity> = entities.insert(Entity { hp: 1 });
    assert_eq!(index1.slot(), unsafe { Slot::from_raw(1) });
    assert_eq!(entities.len(), 2);

    let removed_entity = entities.remove(index0);
    assert_eq!(entities.len(), 1);
    assert_eq!(removed_entity, Some(Entity { hp: 0 }));

    let index0: Index<Entity> = entities.insert(Entity { hp: 10 });
    assert_eq!(entities.len(), 2);
    assert_eq!(index0.slot(), unsafe { Slot::from_raw(0) });

    // extend
    let index2: Index<Entity> = entities.insert(Entity { hp: 2 });
    assert_eq!(index2.slot(), unsafe { Slot::from_raw(2) });
    assert_eq!(entities.len(), 3);
    assert_eq!(entities.capacity(), 4);

    let index3: Index<Entity> = entities.insert(Entity { hp: 3 });
    assert_eq!(index3.slot(), unsafe { Slot::from_raw(3) });
    assert_eq!(entities.len(), 4);
    assert_eq!(entities.capacity(), 4);

    // extend
    let index4: Index<Entity> = entities.insert(Entity { hp: 4 });
    assert_eq!(index4.slot(), unsafe { Slot::from_raw(4) });
    assert_eq!(entities.len(), 5);
    assert_eq!(entities.capacity(), 8);

    let index4_2: Index<Entity> = entities.replace(index4, Entity { hp: 400 });
    assert_eq!(index4_2.slot(), unsafe { Slot::from_raw(4) });
    assert_eq!(index4_2.gen(), NonZeroU32::new(3).unwrap());
    assert_eq!(entities.get(index4), None);
    assert_eq!(entities.remove(index4), None);
    assert_eq!(entities.invalidate(index4), Some(index4_2));
}

use std::collections::HashSet;

#[test]
fn drain() {
    let mut arena: Arena<i32> = Arena::with_capacity(2);
    let one = arena.insert(1);
    let two = arena.insert(2);

    let mut drained_pairs = HashSet::new();
    {
        let mut drain = arena.drain();
        assert_eq!(drain.size_hint(), (2, Some(2)));

        drained_pairs.insert(drain.next().unwrap());
        assert_eq!(drain.size_hint(), (1, Some(1)));
    }

    assert_eq!(arena.len(), 0);
    assert_eq!(arena.capacity(), 2);
    assert_eq!(drained_pairs.len(), 1);

    let one_prime = arena.insert(1);
    let two_prime = arena.insert(2);

    assert_eq!(arena.len(), 2);
    assert_eq!(arena.capacity(), 2);
    assert_eq!(arena.get(one_prime), Some(&1));
    assert_eq!(arena.get(two_prime), Some(&2));
    assert_eq!(arena.get(one), None);
    assert_eq!(arena.get(two), None);
}

#[test]
fn retain() {
    let mut arena = Arena::<i32>::new();

    for i in 0..100 {
        arena.insert(i);
    }

    arena.retain(|_index, &mut i| i % 2 == 1);

    assert!(arena.items().all(|data| data % 2 == 1));
    assert_eq!(arena.len(), 50);
}

#[test]
fn entry_bind() {
    let mut arena = Arena::<i32>::new();

    for i in 0..100 {
        arena.insert(i);
    }

    // simulate `drain_filter::<Vec<_>>()`
    let mut drain = Vec::new();
    for entry in arena.entries_mut() {
        if entry.get() % 2 == 0 {
            let data = entry.remove();
            drain.push(data);
        }
    }

    assert!(drain.iter().all(|data| data % 2 == 0));
    assert!(arena.items().all(|data| data % 2 == 1));
}
