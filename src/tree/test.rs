use super::*;

use crate::tree::iter::*;

use std::fmt::{Debug, Display, Write};

// --------------------------------------------------------------------------------
// iter.rs

// trace_macros!(true);

#[test]
fn tree_macro_test() {
    use crate::tree;

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

    let expected = r##"
0
1
  10
  11
    100
    101
  12
"##;

    self::test_tree_manual_walk(tree.root_nodes(), expected);
}

#[test]
fn manual_traverse() {
    let mut tree = Tree::<&'static str>::default();

    let x = tree.insert("x");
    let _x0 = x.attach("x0", &mut tree).unwrap();
    let _x1 = x.attach("x1", &mut tree).unwrap();

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
    let _x0_0 = x0.attach("x0_0", &mut tree).unwrap();
    let x0_1 = x0.attach("x0_1", &mut tree).unwrap();

    let _x1 = tree.insert("x1");
    let _x0_1_0 = x0_1.attach("x0_1_0", &mut tree).unwrap();

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
    let mut tree: Tree<&'static str> = tree! {
        "x0",
        "x1", {
            "x1-0",
            "x1-1", {
                "x1-1-0",
                "x1-1-1",
                "x1-1-2",
            },
            "x1-2",
        },
        "x2",
    };

    let expected = r##"
x0
x1
  x1-0
  x1-1
    x1-1-0
    x1-1-1
    x1-1-2
  x1-2
x2
"##;

    self::test_tree_traverse(tree.traverse_root_nodes(), expected);

    let mut nodes = tree.root_nodes_mut();
    nodes.next().unwrap();
    nodes
        .bind()
        .unwrap()
        .children()
        .nth(1)
        .unwrap()
        .remove_children();
    drop(nodes);

    let expected = r##"
x0
x1
  x1-0
  x1-1
  x1-2
x2
"##;
    self::test_tree_traverse(tree.traverse_root_nodes(), expected);

    let mut nodes = tree.root_nodes_mut();
    nodes.next().unwrap();
    nodes.bind().unwrap().remove();
    drop(nodes);

    let expected = r##"
x0
x2
"##;
    self::test_tree_traverse(tree.traverse_root_nodes(), expected);
}

// --------------------------------------------------------------------------------
// utilities

fn test_tree_manual_walk<'a, T: Display + Debug, D, G: Gen>(
    siblings: impl Iterator<Item = NodeRef<'a, T, D, G>>,
    expected: &str,
) where
    T: 'a,
    D: 'a,
{
    let mut buf = String::new();
    on_children(&mut buf, 0, siblings);
    assert_eq!(buf.trim(), expected.trim());

    fn on_children<'a, T: Display + Debug, D, G: Gen>(
        buf: &mut String,
        indent: usize,
        siblings: impl Iterator<Item = NodeRef<'a, T, D, G>>,
    ) where
        T: 'a,
        D: 'a,
    {
        for node in siblings {
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
