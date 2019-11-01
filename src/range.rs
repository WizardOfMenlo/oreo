use serde::Serialize;
use std::ops::Range;

pub trait Ranged {
    fn range(&self) -> &Range<usize>;

    fn span<'a>(&self, src: &'a str) -> &'a str {
        &src[self.range().clone()]
    }
}

/// An object that stores metadate about its source location
#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize)]
pub struct RangedObject<T> {
    inner: T,
    range: Range<usize>,
}

impl<T> RangedObject<T> {
    pub fn new(inner: T, range: Range<usize>) -> Self {
        Self { inner, range }
    }

    pub fn map<K>(self, f: impl FnOnce(T) -> K) -> RangedObject<K> {
        RangedObject {
            inner: f(self.inner),
            range: self.range,
        }
    }

    pub fn inner(&self) -> &T {
        &self.inner
    }

    pub fn take_inner(self) -> T {
        self.inner
    }

    pub fn take_range(self) -> Range<usize> {
        self.range
    }
}

impl<T> Ranged for RangedObject<T> {
    fn range(&self) -> &Range<usize> {
        &self.range
    }
}
