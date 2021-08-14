use super::*;

use std::fmt::{Display, Write};

fn drt_test<T: Display, D, G: Gen>(drt: &Drt<T, D, G>, expected: &str) {
    let mut s = String::new();
    self::drt_string(&mut s, 0, drt, drt.root().iter().cloned());
    assert_eq!(s.trim_start(), expected.trim_start());
}

fn drt_string<T: Display, D, G: Gen>(
    s: &mut String,
    indent: usize,
    drt: &Drt<T, D, G>,
    children: impl Iterator<Item = NodeId<T, D, G>>,
) {
    for child in children {
        let node = &drt[child];
        writeln!(s, "{}{}", "  ".repeat(indent), node.data()).unwrap();

        self::drt_string(s, indent + 1, drt, node.child_indices().iter().cloned());
    }
}

#[test]
fn drt() {
    let mut drt = Drt::<&'static str>::default();

    let x = drt.insert("x");
    let _x1 = x.append(&mut drt, "x1");
    let _x2 = x.append(&mut drt, "x2");

    self::drt_test(
        &drt,
        r##"
x
  x1
  x2
"##,
    );
}
