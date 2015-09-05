#![feature(default_type_parameter_fallback)]

pub mod monad;
pub mod parsers;

pub trait Parser<'a, I, T, E> {
    fn parse(self, &'a [I]) -> State<'a, I, T, E>;
}

impl<'a, I, T, E, F> Parser<'a, I, T, E> for F
  where F: FnOnce(&'a [I]) -> State<'a, I, T, E> {
    fn parse(self, i: &'a [I]) -> State<'a, I, T, E> {
        self(i)
    }
}

/// Constructs a parser from a closure.
/// 
/// This function is necessary for inference at time.
fn parser<'a, 'p, I, T, E, F>(p: F) -> impl Parser<'a, I, T, E> + 'p
  where F: FnOnce(&'a [I]) -> State<'a, I, T, E> + 'p {
    p
}

#[derive(Debug, Eq, PartialEq)]
#[must_use]
pub enum State<'a, I, T, E>
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

#[cfg(feature = "verbose_error")]
mod error {
    //! This is a private module to contain the more verbose error type as well as adapters for
    //! using it.
    //! 
    //! All adapters are #inline(always) and will construct the appropriate error type.
    use std::fmt;

    use ::State;

    #[derive(Debug, Eq, PartialEq)]
    pub enum Error<I> {
        Expected(I),
        Unexpected,
        String(Vec<I>),
    }

    impl<I> fmt::Display for Error<I>
      where I: fmt::Debug {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            match *self {
                Error::Expected(ref c) => write!(f, "expected {:?}", *c),
                Error::Unexpected      => write!(f, "unexpected"),
                Error::String(ref s)   => write!(f, "expected {:?}", *s),
            }
        }
    }

    #[inline(always)]
    pub fn unexpected<I>() -> Error<I> {
        Error::Unexpected
    }

    #[inline(always)]
    pub fn expected<'a, I>(i: I) -> Error<I> {
        Error::Expected(i)
    }


    #[inline(always)]
    pub fn string<'a, 'b, I, T>(buffer: &'a [I], _offset: usize, expected: &'b [I]) -> State<'a, I, T, Error<I>>
      where I: Copy {
        State::Error(buffer, Error::String(expected.to_vec()))
    }
}

#[cfg(not(feature = "verbose_error"))]
mod error {
    //! This is a private module to contain the smaller error type as well as adapters for using
    //! it.
    //! 
    //! All adapters are #inline(always), and will just noop the data.
    use std::marker::PhantomData;

    use ::State;

    #[derive(Debug, Eq, PartialEq)]
    pub struct Error<I>(PhantomData<I>);

    #[inline(always)]
    pub fn unexpected<I>() -> Error<I> {
        Error(PhantomData)
    }

    #[inline(always)]
    pub fn expected<'a, I>(_: I) -> Error<I> {
        Error(PhantomData)
    }

    #[inline(always)]
    pub fn string<'a, 'b, I, T>(buffer: &'a [I], offset: usize, _expected: &'b [I]) -> State<'a, I, T, Error<I>>
      where I: Copy {
        State::Error(&buffer[offset..], Error(PhantomData))
    }
}
