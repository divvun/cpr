use std::{
    collections::{BTreeMap, VecDeque},
    hash::Hash,
    ops::{BitAnd, BitOr, Range},
};

/// An ordered, non-overlapping set of `Range<usize>` associated to values.
///
/// Initially, the state looks like this:
///
/// ```text
/// stack: [A], vec: [(0, A)]
/// -------------------------------------------------
/// [.A.......
/// ^
/// 0
/// ```
///
/// Where `A` is the default value for the value type `V` - it's important
/// for this to be a zero value.
///
/// `push(n, B)` pushes a new range onto the stack whose value is the AND
/// combination of the active range's value, and the specified value:
///
/// ```text
/// stack: [A, A&B], vec: [(0, A), (n, A&B)]
/// -------------------------------------------------
/// [..A..|.A&B.....
/// ^     ^
/// 0     n
/// ```
///
/// `push(m, C)` further grows the stack:
///
/// ```text
/// stack: [A, A&B, (A&B)&C], vec: [(0, A), (n, A&B), (m, (A&B)&C)]
/// -------------------------------------------------
/// [..A..|..A&B..|.(A&B)&C.......
/// ^             ^
/// 0             m
/// ```
///
/// `pop(k)` marks the end of the last-pushed range, pops its value off the stack
/// and starts a range with the value now on top of the stack:
///
/// ```text
/// stack: [A, A&B], vec: [(0, A), (n, A&B), (m, (A&B)&C), (k, A&B)]
/// -------------------------------------------------
/// [..A..|..A&B..|..(A&B)&C..|.A&B....
/// ^                         ^
/// 0                         k
/// ```
///
/// Another `pop(j)`:
///
/// ```text
/// stack: [A], vec: [(0, A), (n, A&B), (m, (A&B)&C), (k, A&B), (j, A)]
/// -------------------------------------------------
/// [..A..|..A&B..|..(A&B)&C..|..A&B..|.A....
/// ^                                 ^
/// 0                                 j
/// ```
///
#[derive(Debug)]
pub struct RangeSet<V: Default> {
    /// Stack of pushed values, last pushed is the current active
    stack: Vec<V>,

    /// Stores `(index, value)` tuples ordered by `index`,
    /// where `index` is the start of a range.
    vec: BTreeMap<usize, V>,
}

impl<V> RangeSet<V>
where
    V: Default + Clone + PartialEq + Eq + Hash + BitAnd<Output = V> + BitOr<Output = V>,
{
    /// Construct a set with a signle range, starting at zero,
    /// with the `V`'s default value
    pub fn new() -> RangeSet<V> {
        let mut set = RangeSet {
            stack: vec![],
            vec: BTreeMap::new(),
        };
        set.vec.insert(0, V::default());
        set
    }

    /// Starts a new range, whose value is AND'd with the
    /// parent's value, ie. go from
    ///
    /// ```text
    /// [.....A.....
    /// ```
    ///
    /// Then `push(n, B)` will add a new range on the stack, starting
    /// at `n`, and with value `A&B`
    ///
    /// ```text
    /// [.....A.....|....A&B....
    /// ^           ^
    /// 0           n
    /// ```
    pub fn push(&mut self, tuple: (usize, V)) {
        let (index, child_value) = tuple;
        assert!(index == 0 || *self.vec.keys().last().unwrap() <= index);

        let parent_value = self.vec.values().last().unwrap().clone();
        let combo = parent_value & child_value;

        self.vec.insert(index, combo.clone());
        self.stack.push(combo);
    }

    /// Checked pop: ends the last-started range
    ///
    /// Given this state:
    ///
    /// ```text
    /// [.....A.....|....A&B....
    /// ```
    ///
    /// Then `pop(n)` will transition to:
    ///
    /// ```text
    /// [.....A....|....A&B....|.....A......
    /// ^          ^           ^
    /// 0          m           n
    /// ```
    ///
    /// Pop is checked in the sense that if `m > n`, this panics.
    pub fn pop(&mut self, index: usize) {
        assert!(self.vec.len() > 0);
        assert!(!self.vec.keys().last().is_none() && *self.vec.keys().last().unwrap() <= index);

        self.stack.pop();
        let prev_key = self.stack.last().cloned().unwrap_or_default();
        self.vec.insert(index, prev_key);
    }

    /// Returns the start index and value of "most recently pushed" range
    pub fn last(&self) -> (usize, &V) {
        self.vec.iter().last().map(|(a, b)| (*a, b)).unwrap()
    }

    /// Returns an iterator over all ranges of this set.
    pub fn iter<'a>(&'a self) -> Iter<'a, V> {
        Iter {
            range_set: self,
            keys: self.vec.keys().copied().collect::<VecDeque<_>>(),
        }
    }
}

/// Iterates over all ranges of a RangeSet
pub struct Iter<'a, V: Default> {
    range_set: &'a RangeSet<V>,
    keys: VecDeque<usize>,
}

impl<'a, V: Default> Iterator for Iter<'a, V> {
    type Item = (Range<usize>, &'a V);

    fn next(&mut self) -> Option<Self::Item> {
        // fencepost problem: keys are posts, but we yield fences
        //
        // ```text
        // 0..........1..........2   keys
        // [    0     |     1    ]    ranges
        // ```
        //
        if self.keys.len() <= 1 {
            return None;
        }

        let first = self.keys.pop_front().unwrap();
        let second = self.keys[0];
        Some((first..second, self.range_set.vec.get(&first).unwrap()))
    }
}
