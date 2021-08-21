use super::*;

use crate::tree::iter::*;

use std::fmt::{Debug, Display, Write};

#[test]
fn manual_traverse() {
    let mut tree = Tree::<&'static str>::default();

    let x = tree.insert("x");
    let _x1 = x.append(&mut tree, "x1").unwrap();
    let _x2 = x.append(&mut tree, "x2").unwrap();

    println!("{:#?}", tree);

    let expected = r##"
x
  x1
  x2
"##;

    self::test_tree_manual_walk(tree.rooted_nodes(), expected);
}

#[test]
fn automatic_traverse() {
    let mut tree = Tree::<&'static str>::default();

    let x = tree.insert("x");
    let _x1 = x.append(&mut tree, "x1").unwrap();
    let _x2 = x.append(&mut tree, "x2").unwrap();

    println!("{:#?}", tree);

    let expected = r##"
x
  x1
  x2
"##;

    self::test_tree_traverse(tree.root_traverse(), expected);
    self::test_tree_traverse(tree.traverse(x), expected);
}

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

    assert_eq!(depth, 0);
    assert_eq!(buf.trim_start(), expected.trim_start());
}
