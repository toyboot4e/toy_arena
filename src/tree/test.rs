use super::*;

use std::fmt::{Display, Write};

fn tree_test<T: Display, D, G: Gen>(tree: &Drt<T, D, G>, expected: &str) {
    let mut s = String::new();
    self::tree_string(&mut s, 0, tree, tree.root().iter().cloned());
    assert_eq!(s.trim_start(), expected.trim_start());
}

fn tree_string<T: Display, D, G: Gen>(
    s: &mut String,
    indent: usize,
    tree: &Drt<T, D, G>,
    children: impl Iterator<Item = NodeId<T, D, G>>,
) {
    for child in children {
        let node = &tree[child];
        writeln!(s, "{}{}", "  ".repeat(indent), node.data()).unwrap();

        self::tree_string(s, indent + 1, tree, node.child_indices().iter().cloned());
    }
}

#[test]
fn tree() {
    let mut tree = Drt::<&'static str>::default();

    let x = tree.insert("x");
    let _x1 = x.append(&mut tree, "x1");
    let _x2 = x.append(&mut tree, "x2");

    self::tree_test(
        &tree,
        r##"
x
  x1
  x2
"##,
    );
}
