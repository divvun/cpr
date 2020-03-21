pub fn variations(len: usize, f: &mut dyn FnMut(Box<dyn Iterator<Item = bool>>)) {
    use std::iter::{empty, once};

    match len {
        0 => f(Box::new(empty())),
        n => {
            for &b in &[true, false] {
                variations(n - 1, &mut |it| {
                    f(Box::new(once(b).chain(it)));
                });
            }
        }
    }
}

#[cfg(test)]
#[test]
fn test_variations() {
    fn test(n: usize, expected: &[&[bool]]) {
        let mut vecs = Vec::<Vec<_>>::new();
        variations(n, &mut |v| vecs.push(v.collect()));
        assert_eq!(&vecs[..], expected);
    }

    test(1, &[&[true], &[false]]);
    test(
        2,
        &[
            &[true, true],
            &[true, false],
            &[false, true],
            &[false, false],
        ],
    );
    test(
        3,
        &[
            &[true, true, true],
            &[true, true, false],
            &[true, false, true],
            &[true, false, false],
            &[false, true, true],
            &[false, true, false],
            &[false, false, true],
            &[false, false, false],
        ],
    );
    test(
        4,
        &[
            &[true, true, true, true],
            &[true, true, true, false],
            &[true, true, false, true],
            &[true, true, false, false],
            &[true, false, true, true],
            &[true, false, true, false],
            &[true, false, false, true],
            &[true, false, false, false],
            &[false, true, true, true],
            &[false, true, true, false],
            &[false, true, false, true],
            &[false, true, false, false],
            &[false, false, true, true],
            &[false, false, true, false],
            &[false, false, false, true],
            &[false, false, false, false],
        ],
    );
}
