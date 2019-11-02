//! Utilities for working with ranges in text

use serde::Serialize;
use std::ops::Range;

/// Trait encompassing all the objects in oreo that have associated
/// span information (i.e. Tokens and Nodes)
pub trait Ranged {
    /// The range the object has
    fn range(&self) -> &Range<usize>;

    /// Get the actual test span
    fn span<'a>(&self, src: &'a str) -> &'a str {
        &src[self.range().clone()]
    }
}

/// An object that stores metadata about its source location
#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize)]
pub struct RangedObject<T> {
    inner: T,
    range: Range<usize>,
}

impl<T> RangedObject<T> {
    /// Create a ranged object from a range
    /// ```
    /// use oreo::range::RangedObject;
    /// let x = 5;
    /// let ranged_x = RangedObject::new(x, 0..10);
    /// assert_eq!(*ranged_x.inner(), x);
    /// ```
    pub fn new(inner: T, range: Range<usize>) -> Self {
        Self { inner, range }
    }

    /// Map a ranged object into one with the same range but different inner
    /// ```
    /// use oreo::range::RangedObject;
    /// let x = 5;
    /// let ranged_x = RangedObject::new(x, 0..10);
    /// assert_eq!(*ranged_x.map(|e| e * e).inner(), 25);
    /// ```
    pub fn map<K>(self, f: impl FnOnce(T) -> K) -> RangedObject<K> {
        RangedObject {
            inner: f(self.inner),
            range: self.range,
        }
    }

    /// Get a reference to the inner object
    pub fn inner(&self) -> &T {
        &self.inner
    }

    /// Take ownership of the inner object
    pub fn take_inner(self) -> T {
        self.inner
    }

    /// Take ownership of the range
    pub fn take_range(self) -> Range<usize> {
        self.range
    }
}

impl<T> Ranged for RangedObject<T> {
    fn range(&self) -> &Range<usize> {
        &self.range
    }
}
