mod mdo;
mod iter;

use std::fmt;

pub mod combinator;
pub mod monad;
pub mod parser;

pub use monad::{
    bind,
    err,
    ret,
};

pub use combinator::{
    count,
    option,
    or,
    many,
    many1,
};

pub use parser::{
    any,
    char,
    satisfy,
    take_while,
    take_while1,
    take_till,
    string,
};

// Only export error publicly.
pub use error::Error;

#[derive(Debug, Eq, PartialEq)]
#[must_use]
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

impl<'a, I, T, E> Parser<'a, I, T, E> {
    /// Applies the function ``f`` on the value contained if the parser is in a success state.
    /// 
    /// ```
    /// use parser::Parser;
    /// use parser::ret;
    /// 
    /// let p: Parser<_, u32, ()> = ret(From::from(b"abc"), 1);
    /// 
    /// assert_eq!(p.map(|i| i + 2).unwrap(), 3);
    /// ```
    pub fn map<F, U>(self, f: F) -> Parser<'a, I, U, E>
      where F: FnOnce(T) -> U {
        bind(self, |m, t| ret(m, f(t)))
    }
}

impl<'a, I, T, E> Parser<'a, I, T, E>
  where T: fmt::Debug {
    pub fn unwrap_err(self) -> E {
        match self.0 {
            State::Item(_, t)    => panic!("called `Parser::unwrap_err` on a parser in a success state: {:?}", t),
            State::Error(_, e)   => e,
            State::Incomplete(_) => panic!("called `Parser::unwrap_err` on a parser in an incomplete state"),
        }
    }
}
impl<'a, I, T, E> Parser<'a, I, T, E>
  where E: fmt::Debug {
    pub fn unwrap(self) -> T {
        match self.0 {
            State::Item(_, t)    => t,
            State::Error(_, e)   => panic!("called `Parser::unwrap` on a parser in an error state: {:?}", e),
            State::Incomplete(_) => panic!("called `Parser::unwrap` on a parser in an incomplete state"),
        }
    }
}

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

pub struct Iter<'a, I, U, E, F>(iter::Iter<'a, I, U, E, F>)
  where I: 'a,
        F: FnMut(Input<'a, I>) -> Parser<'a, I, U, E>;

impl<'a, I: 'a + Copy, T, E, F> Iter<'a, I, T, E, F>
  where F: FnMut(Input<'a, I>) -> Parser<'a, I, T, E> {
    #[inline]
    pub fn new(buffer: &'a [I], f: F) -> Iter<'a, I, T, E, F> {
        Iter(iter::Iter::new(buffer, f))
    }
}

impl<'a, I: 'a + Copy, T, E, F> Iterator for Iter<'a, I, T, E, F>
  where F: FnMut(Input<'a, I>) -> Parser<'a, I, T, E> {
    type Item = T;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.0.next()
    }
}

#[cfg(feature = "verbose_error")]
mod error {
    //! This is a private module to contain the more verbose error type as well as adapters for
    //! using it.
    //! 
    //! All adapters are #inline(always) and will construct the appropriate error type.
    use std::fmt;

    use ::Parser;
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
    pub fn string<'a, 'b, I, T>(buffer: &'a [I], _offset: usize, expected: &'b [I]) -> Parser<'a, I, T, Error<I>>
      where I: Copy {
        return Parser(State::Error(buffer, Error::String(expected.to_vec())));
    }
}

#[cfg(not(feature = "verbose_error"))]
mod error {
    //! This is a private module to contain the smaller error type as well as adapters for using
    //! it.
    //! 
    //! All adapters are #inline(always), and will just noop the data.
    use std::marker::PhantomData;

    use ::Parser;
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
    pub fn string<'a, 'b, I, T>(buffer: &'a [I], offset: usize, _expected: &'b [I]) -> Parser<'a, I, T, Error<I>>
      where I: Copy {
        // Parser(State::Error(&buffer[offset..], Error::Unexpected));
        Parser::<I, T, Error<I>>(State::Error(&buffer[offset..], Error(PhantomData)))
    }
}
