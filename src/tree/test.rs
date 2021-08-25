use super::*;

use crate::tree::iter::*;

use std::fmt::{Debug, Display, Write};

// --------------------------------------------------------------------------------
// iter.rs

#[test]
fn manual_traverse() {
    let mut tree = Tree::<&'static str>::default();

    let x = tree.insert("x");
    let _x0 = x.attach(&mut tree, "x0").unwrap();
    let _x1 = x.attach(&mut tree, "x1").unwrap();

    let expected = r##"
x
  x0
  x1
"##;

    self::test_tree_manual_walk(tree.root_nodes(), expected);
}

#[test]
fn automatic_traverse() {
    let mut tree = Tree::<&'static str>::default();

    let x0 = tree.insert("x0");
    let _x0_0 = x0.attach(&mut tree, "x0_0").unwrap();
    let x0_1 = x0.attach(&mut tree, "x0_1").unwrap();
    let _x1 = tree.insert("x1");
    let _x0_1_0 = x0_1.attach(&mut tree, "x0_1_0").unwrap();

    // traverse
    let expected = r##"
x0
  x0_0
  x0_1
    x0_1_0
x1
"##;

    self::test_tree_traverse(tree.traverse_root_nodes(), expected);
    self::test_tree_traverse(tree.traverse(x0), expected);

    // preorder
    let expected = r##"
x0
  x0_0
  x0_1
    x0_1_0
"##;
    self::test_tree_traverse(tree.subtree(x0), expected);
}

// --------------------------------------------------------------------------------
// iter_mut.rs

#[test]
fn tree_remove() {
    let mut tree = Tree::<&'static str>::default();

    let x = tree.insert("x");
    let x0 = x.attach(&mut tree, "x0").unwrap();
    let x1 = x.attach(&mut tree, "x1").unwrap();
    let x1_0 = x1.attach(&mut tree, "x1_0").unwrap();
    let xx = tree.insert("xx");
    let x1_1 = x1.attach(&mut tree, "x1_1").unwrap();

    let expected = r##"
x
  x0
  x1
    x1_0
    x1_1
xx
"##;
    println!("{:#?}", tree);
    self::test_tree_traverse(tree.traverse_root_nodes(), expected);

    tree.bind(x1).unwrap().remove();

    assert_eq!(tree.data(x), Some(&"x"));
    assert_eq!(tree.data(x0), Some(&"x0"));
    assert_eq!(tree.data(x1), None);
    assert_eq!(tree.data(x1_0), None);
    assert_eq!(tree.data(x1_1), None);

    let expected = r##"
x
  x0
xx
"##;
    self::test_tree_traverse(tree.traverse_root_nodes(), expected);
    self::test_tree_traverse(tree.traverse(x), expected);
}

// --------------------------------------------------------------------------------
// utilities

fn test_tree_manual_walk<'a, T: Display + Debug, D, G: Gen>(
    preorder: impl Iterator<Item = NodeRef<'a, T, D, G>>,
    expected: &str,
) where
    T: 'a,
    D: 'a,
{
    let mut buf = String::new();
    on_children(&mut buf, 0, preorder);
    assert_eq!(buf.trim(), expected.trim());

    fn on_children<'a, T: Display + Debug, D, G: Gen>(
        buf: &mut String,
        indent: usize,
        preorder: impl Iterator<Item = NodeRef<'a, T, D, G>>,
    ) where
        T: 'a,
        D: 'a,
    {
        for node in preorder {
            writeln!(buf, "{}{}", "  ".repeat(indent), node.data()).unwrap();
            on_children(buf, indent + 1, node.children())
        }
    }
}

fn test_tree_traverse<T: Display, D, G: Gen>(traverse: iter::Traverse<T, D, G>, expected: &str) {
    let mut buf = String::new();

    let mut depth = 0;

    for item in traverse {
        match item {
            TraverseItem::Node(sibling) => {
                writeln!(buf, "{}{}", "  ".repeat(depth), sibling.data()).unwrap();
            }
            TraverseItem::Child(child) => {
                depth += 1;
                writeln!(buf, "{}{}", "  ".repeat(depth), child.data()).unwrap();
            }
            TraverseItem::EndChildren => {
                depth -= 1;
            }
        }
    }

    assert_eq!(depth, 0, "tree:{}", buf.trim_start());
    assert_eq!(buf.trim_start(), expected.trim_start());
}
