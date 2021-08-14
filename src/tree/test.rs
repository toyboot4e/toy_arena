use super::*;

use crate::tree::iter::*;

use std::fmt::{Display, Write};

fn tree_test<'a, T: Display, D, G: Gen>(
    root_children: impl Iterator<Item = NodeRef<'a, T, D, G>>,
    expected: &str,
) where
    T: 'a,
    // FIXME: this requirement should be relaxed
    D: 'a,
{
    let mut buf = String::new();
    self::tree_string_siblings(&mut buf, 0, root_children);
    assert_eq!(buf.trim_start(), expected.trim_start());
}

fn tree_string_siblings<'a, T: Display, D, G: Gen>(
    buf: &mut String,
    indent: usize,
    siblings: impl Iterator<Item = NodeRef<'a, T, D, G>>,
) where
    T: 'a,
    D: 'a,
{
    for s in siblings {
        writeln!(buf, "{}{}", "  ".repeat(indent), s.data()).unwrap();
        self::tree_string_children(buf, indent + 1, s);
    }
}

fn tree_string_children<'a, T: Display, D, G: Gen>(
    buf: &mut String,
    indent: usize,
    bind: NodeRef<'a, T, D, G>,
) {
    for child in bind.children() {
        writeln!(buf, "{}{}", "  ".repeat(indent), child.data()).unwrap();
        self::tree_string_siblings(buf, indent, child.children())
    }
}

#[test]
fn tree() {
    let mut tree = Tree::<&'static str>::default();

    let x = tree.insert("x");
    let _x1 = x.append(&mut tree, "x1").unwrap();
    let _x2 = x.append(&mut tree, "x2").unwrap();

    println!("{:#?}", tree);

    self::tree_test(
        tree.root(),
        r##"
x
  x1
  x2
"##,
    );
}
