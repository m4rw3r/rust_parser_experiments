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

#[cfg(feature = "verbose_error")]
mod error {
    //! This is a private module to contain the more verbose error type as well as adapters for
    //! using it.
    //! 
    //! All adapters are #inline(always) and will construct the appropriate error type.
    use std::fmt;

    #[derive(Debug, Eq, PartialEq)]
    pub enum Error<I> {
        Expected(I),
        Unexpected,
        String(Vec<I>),
    }

    impl<I> fmt::Display for Error<I>
      where I: fmt::Debug {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            match self {
                &Error::Expected(ref c) => write!(f, "expected {:?}", *c),
                &Error::Unexpected      => write!(f, "unexpected"),
                &Error::String(ref s)   => write!(f, "expected {:?}", *s),
            }
        }
    }

    #[inline(always)]
    pub fn expected<'a, I>(i: I) -> Error<I> {
        Error::Expected(i)
    }


    #[inline(always)]
    pub fn string<I>(c: &[I]) -> Error<I>
      where I: Copy {
        Error::String(c.to_vec())
    }
}

#[cfg(not(feature = "verbose_error"))]
mod error {
    //! This is a private module to contain the smaller error type as well as adapters for using
    //! it.
    //! 
    //! All adapters are #inline(always), and will just noop the data.
    use std::marker::PhantomData;

    #[derive(Debug, Eq, PartialEq)]
    pub enum Error<I> {
        Unexpected,
        Many1(PhantomData<I>),
    }

    #[inline(always)]
    pub fn expected<'a, I>(_: I) -> Error<I> {
        Error::Unexpected
    }

    #[inline(always)]
    pub fn many1<'a, I>() -> Error<I> {
        Error::Many1(PhantomData)
    }
}
