#[allow(unused_attributes)]
#[cfg_attr(feature = "verbose_error", path = "error_verbose.rs")]
mod error;
mod mdo;

pub mod combinator;
pub mod iter;
pub mod monad;
pub mod parser;

pub use monad::{
    bind,
    err,
    ret,
};

// Only export error publicly.
pub use error::Error;

#[derive(Debug, Eq, PartialEq)]
enum State<'a, I, T, E>
  where I: 'a {
    /// Success state, slice is the slice of the input data starting just after the last successful
    /// parser.
    Item(&'a [I], T),
    /// Error state, slice is the slice of the input data from the position where the error
    /// occurred.
    Error(&'a [I], E),
    // Do not include data-slice here as we do not need to actually return the part which is
    // incomplete. The parse has to restart anyway from the index where it was.
    Incomplete(usize),
}

// Newtype wrapper to avoid exposing internal state.
#[must_use]
pub struct Parser<'a, I, T, E>(State<'a, I, T, E>)
  where I: 'a;

// Newtype wrapper around parser input to avoid arbitrary slice to replace our actual input.
#[must_use]
pub struct Input<'a, I>(&'a [I])
  where I: 'a;

#[inline]
impl<'a, I, R> From<&'a R> for Input<'a, I>
  where I: 'a + Copy,
        R: 'a + AsRef<[I]>{
    fn from(buf: &'a R) -> Input<'a, I> {
        Input(buf.as_ref())
    }
}
