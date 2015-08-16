//! ... is an eager zero-copy monadic parser combinator operating on arbitrary slices of data.
//!
mod mdo;

pub mod iter;
pub mod combinators;
pub mod parsers;

use std::error;
use std::fmt;
use std::any;

pub use functor::map;
pub use monad::{bind, err, ret};

pub use combinators::{
    option,
    count,
    or,
    many,
    many1,
    skip_many,
};
pub use parsers::{
    any,
    char,
    not_char,
    satisfy,
    peek,
    take,
    take_till,
    take_while,
    take_while1,
    take_remainder,
    string,
};

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Error<T: Copy> {
    /// Token did not satisfy the given expression.
    NotSatisfy(T),
    /// Did not expect the given token.
    Unexpected(T),
    /// Expected the first token, got the second token instead.
    Expected(T, T),
    /// Expected anything but ``T``.
    NotExpect(T),
    /// Many1 combinator failed to parse at least one item.
    // TODO: Better error, wrap the inner error
    Many1Fail,
    /// Parser did not match count times.
    ExpectedCount(usize),
}

impl<T: fmt::Debug + Copy> fmt::Display for Error<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            &Error::NotSatisfy(t)    => write!(f, "'{:?}' did not satisfy condition", t),
            &Error::Unexpected(t)    => write!(f, "unexpected '{:?}' while parsing", t),
            &Error::Expected(e, a)   => write!(f, "expected '{:?}', got '{:?}'", e, a),
            &Error::NotExpect(e)     => write!(f, "expected anything but '{:?}', got {:?}", e, e),
            &Error::Many1Fail        => write!(f, "expected at least one match, got 0"),
            &Error::ExpectedCount(n) => write!(f, "expected {} matches", n),
        }
    }
}

impl<T: any::Any + fmt::Debug + Copy> error::Error for Error<T> {
    fn description(&self) -> &str {
        match self {
            &Error::NotSatisfy(_)    => "The encountered item did not satisfy an expression",
            &Error::Unexpected(_)    => "An unexpected character was encountered",
            &Error::Expected(_, _)   => "Expected a certain character, got another",
            &Error::NotExpect(_)     => "Expected any character but one, got the one",
            &Error::Many1Fail        => "Expected at least one match, got 0",
            &Error::ExpectedCount(_) => "Did not match a parser exactly a certain amount of times",
        }
    }
}

/// Internal 3-variant Result to also represent incomplete
#[derive(Debug, Eq, PartialEq)]
#[must_use]
enum State<'a, I: 'a + Copy, T, E> {
    Ok(T),
    Err(&'a [I], E),
    /// Incomplete parsing, remaining data and the requested length
    Incomplete(&'a [I], usize)
}

/// The main parser data-type, contains the current fragment to be parsed and the current
/// parser-state.
#[derive(Debug)]
#[must_use]
pub struct Parser<'a, I: 'a + Copy, T, E>(&'a [I], State<'a, I, T, E>);

/// A parser state where no data or error is present.
#[must_use]
pub type Empty<'a, I: 'a + Copy> = Parser<'a, I, (), ()>;

impl<'a, I: 'a + Copy, T, E> State<'a, I, T, E> {
    #[inline]
    fn map<F, U>(self, f: F) -> State<'a, I, U, E>
      where F: FnOnce(T) -> U {
        match self {
            State::Ok(t)            => State::Ok(f(t)),
            State::Err(i, t)        => State::Err(i, t),
            State::Incomplete(i, r) => State::Incomplete(i, r),
        }
    }

    #[inline]
    fn is_good(&self) -> bool {
        match *self {
            State::Ok(_) => true,
            _            => false,
        }
    }

    #[inline]
    fn map_err<F, O>(self, f: F) -> State<'a, I, T, O>
      where F: FnOnce(E) -> O {
        match self {
            State::Ok(t)            => State::Ok(t),
            State::Err(i, t)        => State::Err(i, f(t)),
            State::Incomplete(i, r) => State::Incomplete(i, r),
        }
    }
}

impl<'a, I: 'a + Copy + fmt::Debug, T, E: error::Error> Parser<'a, I, T, E> {
    /// Extracts the contained type if the parser is in a success-state, panics otherwise.
    #[inline]
    pub fn unwrap(self) -> T {
        match self.1 {
            State::Ok(t)            => t,
            State::Err(i, e)        => panic!("parser error: {} at {:?}", e, i),
            State::Incomplete(_, r) => panic!("parser is incomplete, expecting at least {} more items", r),
        }
    }
}

pub mod functor {
    //! Functor implementation for ``Parser``.
    use ::Parser;

    impl<'a, I: 'a + Copy, T, E> Parser<'a, I, T, E> {
        /// Applies the function ``f`` to the wrapped value if the parser is in a success state, otherwise
        /// does nothing.
        /// 
        /// ```
        /// use parser::{Error, Parser, ret};
        /// 
        /// let p: Parser<_, usize, Error<()>> = ret(From::from(b"dummy"), 1234);
        /// 
        /// assert_eq!(p.map(|i| i + 20).unwrap(), 1254);
        /// ```
        #[inline]
        pub fn map<F, U>(self, f: F) -> Parser<'a, I, U, E>
          where F: FnOnce(T) -> U {
            Parser(self.0, self.1.map(f))
        }
    }

    /// Applies the function ``f`` to the wrapped value if the parser is in a success state, otherwise
    /// does nothing.
    /// 
    /// ```
    /// use parser::{Error, Parser, map, ret};
    /// 
    /// let p: Parser<_, usize, Error<()>> = ret(From::from(b"dummy"), 1234);
    /// 
    /// assert_eq!(map(p, |i| i + 20).unwrap(), 1254);
    /// ```
    #[inline]
    pub fn map<'a, I: 'a + Copy, T, E, F, U>(m: Parser<'a, I, T, E>, f: F) -> Parser<'a, I, U, E>
      where F: FnOnce(T) -> U {
        m.map(f)
    }
}

pub mod monad {
    //! Monad implementation for ``Parser``.
    //! 
    //! This monad implementation requires manual threading of the monad state to the nested
    //! function ``f`` given to ``bind``. It is necessary since it is an eager monad and the nested
    //! function ``f`` cannot return an action to be completed later in any effective way while
    //! still allowing for ``rustc`` to optimize the code well. I did an earlier experiment with
    //! laziness built-in, but that required lots of stakcking of nested structures to accomplish
    //! the same thing instead of stacking functions. It was not at all as easy to use and usually
    //! resulted in unwieldy stacks of structs.
    //! 
    //! Equivalence with Haskell's ``Monad`` typeclass:
    //! 
    //! ```ignore
    //! f >>= g   ===  bind(f(m), g)
    //! f >> g    ===  bind(f(m), |m, _| g(m))
    //! return a  ===  ret(m, a)
    //! fail a    ===  err(m, a)
    //! ```
    //! 
    //! Do-notation is provided by the macro ``mdo!``.
    use ::Empty;
    use ::State;
    use ::Parser;

    /// Applies the function ``f`` on the value in the parser ``m``, the first parameter to ``f`` is
    /// the context of the parser, to be used for continued processing and the second parameter is the
    /// value contained in ``m``.
    #[inline]
    pub fn bind<'a, I: 'a + Copy, T, E, F, U, O>(m: Parser<'a, I, T, E>, f: F) -> Parser<'a, I, U, O>
      where O: From<E>,
            F: FnOnce(Empty<'a, I>, T) -> Parser<'a, I, U, O> {
        match m.1 {
            State::Ok(s) => {
                let r = f(Parser(m.0, State::Ok(())), s);

                // We rollback if the parser ``f`` failed
                Parser(if r.1.is_good() { r.0 } else { m.0 }, r.1.map_err(From::from))
            },
            State::Err(i, e)        => Parser(m.0, State::Err(i, From::from(e))),
            State::Incomplete(i, r) => Parser(m.0, State::Incomplete(i, r))
        }
    }

    /// Constructs a success value from the given ``value``.
    /// 
    /// Currently this function may need type-annotation for the third type-parameter ``E`` because
    /// the error-value is unconstrained by this function. When wrapped in smaller functions
    /// inference should be able to use the return-value of the function to infer return and error
    /// types.
    /// 
    /// ```
    /// use parser::{Parser, Error, ret};
    /// 
    /// let p = From::from(b"test");
    /// 
    /// assert_eq!(ret::<_, _, Error<u8>>(p, "data").unwrap(), "data");
    /// ```
    #[inline]
    pub fn ret<'a, I: 'a + Copy, T, E>(m: Empty<'a, I>, value: T) -> Parser<'a, I, T, E> {
        Parser(m.0, State::Ok(value))
    }

    /// Constructs an error value from the given error ``err``.
    /// 
    /// Currently this function may need type-annotation for the second type-parameter ``T`` because
    /// the success-value is unconstrained by this function. When wrapped in smaller functions
    /// inference should be able to use the return-value of the function to infer return and error
    /// types.
    /// 
    /// ```should_panic
    /// use parser::{Parser, Error, err};
    /// 
    /// let p = From::from(b"test");
    /// 
    /// assert_eq!(err::<_, &[u8], _>(p, Error::Unexpected(2)).unwrap(), b"data");
    /// ```
    #[inline]
    pub fn err<'a, I: 'a + Copy, T, E>(m: Empty<'a, I>, err: E) -> Parser<'a, I, T, E> {
        Parser(m.0, State::Err(m.0, err))
    }
}

/// Constructs a parser monad for parsing the data in the buffer supplied to ``From::from()``.
/// 
/// ```
/// use parser::{Parser, take};
/// 
/// let mut v = Vec::new();
/// 
/// v.push(b'a');
/// v.push(b'b');
/// 
/// let p = From::from(&v);
/// 
/// assert_eq!(take(p, 2).unwrap(), b"ab");
/// ```
#[inline]
impl<'a, I, R> From<&'a R> for Empty<'a, I>
  where I: 'a + Copy,
        R: 'a + AsRef<[I]>{
    fn from(buf: &'a R) -> Empty<'a, I> {
        Parser(buf.as_ref(), State::Ok(()))
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use super::State;

    #[test]
    fn test_bind() {
        let mut d = None;
        let m = Parser(b"abc", State::Ok("a"));

        let Parser(buf, state) = bind(m, |m, p| {
            d = Some(p);

            ret::<_, _, ()>(m, "test")
        });

        assert_eq!(buf, b"abc");
        assert_eq!(state, State::Ok("test"));
        assert_eq!(d, Some("a"));
    }

    #[test]
    fn test_ret() {
        let Parser(buf, d) = ret::<_, usize, usize>(From::from(b"abc"), 123);

        assert_eq!(buf, b"abc");
        assert_eq!(d, State::Ok(123));
    }

    #[test]
    fn test_err() {
        let Parser(buf, d) = err::<_, usize, usize>(From::from(b"abc"), 123);

        assert_eq!(buf, b"abc");
        assert_eq!(d, State::Err(b"abc", 123));
    }

    #[test]
    fn test_map() {
        let m1: Parser<_, usize, usize>   = Parser(b"abc", State::Ok(123));
        let m2: Parser<_, usize, usize>   = Parser(b"abc", State::Err(b"def", 321));
        let m3: Parser<_, usize, usize>   = Parser(b"abc", State::Incomplete(b"def", 1));

        let Parser(buf, d)  = map(m1, |data| data + 1);

        assert_eq!(buf, b"abc");
        assert_eq!(d, State::Ok(124));

        let Parser(buf, d) = map(m2, |data| data + 1);

        assert_eq!(buf, b"abc");
        assert_eq!(d, State::Err(b"def", 321));

        let Parser(buf, d) = map(m3, |data| data + 1);

        assert_eq!(buf, b"abc");
        assert_eq!(d, State::Incomplete(b"def", 1));
    }

    #[test]
    fn test_map2() {
        let m1: Parser<_, usize, usize>   = Parser(b"abc", State::Ok(123));
        let m2: Parser<_, usize, usize>   = Parser(b"abc", State::Err(b"def", 321));
        let m3: Parser<_, usize, usize>   = Parser(b"abc", State::Incomplete(b"def", 1));

        let Parser(buf, d)  = m1.map(|data| data + 1);

        assert_eq!(buf, b"abc");
        assert_eq!(d, State::Ok(124));

        let Parser(buf, d) = m2.map(|data| data + 1);

        assert_eq!(buf, b"abc");
        assert_eq!(d, State::Err(b"def", 321));

        let Parser(buf, d) = m3.map(|data| data + 1);

        assert_eq!(buf, b"abc");
        assert_eq!(d, State::Incomplete(b"def", 1));
    }
}
