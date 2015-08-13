use std::ops;
use std::fmt;
use std::iter::FromIterator;

#[derive(Debug)]
pub enum Error<T> {
    DidNotSatisfy(T),
    StrError(&'static str),
}

impl<T> From<&'static str> for Error<T> {
    fn from(s: &'static str) -> Error<T> {
        Error::StrError(s)
    }
}

/// Internal 3-variant Result to also represent incomplete
#[derive(Debug)]
#[must_use]
enum State<T, E> {
    Ok(T),
    Err(E),
    Incomplete(usize)
}

use self::State::*;

impl<T, E> State<T, E> {
    fn map<F, U>(self, f: F) -> State<U, E>
      where F: FnOnce(T) -> U {
        match self {
            Ok(t)         => Ok(f(t)),
            Err(t)        => Err(t),
            Incomplete(i) => Incomplete(i),
        }
    }

    fn is_good(&self) -> bool {
        match *self {
            Ok(_) => true,
            _     => false,
        }
    }

    fn map_err<F, O>(self, f: F) -> State<T, O>
      where F: FnOnce(E) -> O {
        match self {
            Ok(t)         => Ok(t),
            Err(t)        => Err(f(t)),
            Incomplete(i) => Incomplete(i),
        }
    }
}

/// The main parser data-type, contains the current fragment to be parsed and the current
/// parser-state.
#[derive(Debug)]
#[must_use]
pub struct Parser<'a, I: 'a + Copy, T, E>(&'a [I], State<T, E>);

impl<'a, I: Copy, E> Parser<'a, I, (), E> {
    #[inline]
    pub fn new(i: &'a [I]) -> Self {
        Parser(i, Ok(()))
    }
}

// Functor
impl<'a, I: 'a + Copy, T, E> Parser<'a, I, T, E> {
    /// Applies the function ``f`` on the internal value, replacing it with the return value of
    /// ``f`,  if parser is in successful state.
    #[inline]
    pub fn map<F, U>(self, f: F) -> Parser<'a, I, U, E>
      where F: FnOnce(T) -> U {
        Parser(self.0, self.1.map(f))
    }
}

// Monad
impl<'a, I: 'a + Copy, T, E> Parser<'a, I, T, E> {
    /// Executes ``f`` in the parser if the parser is in a success state, first parameter provided
    /// to ``f`` is the current result and the second parameter is a parser instance to be used for
    /// continued parsing and monadic chaining.
    /// 
    /// ```
    /// let r = parser::from_slice("test".as_bytes())
    ///     .get()
    ///     .bind(|c, p| if c == b't' {
    ///             p.ret("This is a success!")
    ///         } else {
    ///             p.err("FAIL") });
    ///
    /// assert_eq!(r.unwrap(), "This is a success!");
    /// ```
    #[inline]
    pub fn bind<F, U, O>(self, f: F) -> Parser<'a, I, U, E>
      where E: From<O>,
            F: FnOnce(T, Parser<'a, I, (), E>) -> Parser<'a, I, U, O> {
        match self.1 {
            Ok(s)         => {
                let r = f(s, Parser(self.0, Ok(())));

                // We rollback if the parser ``f`` failed
                Parser(if r.1.is_good() { r.0 } else { self.0 }, r.1.map_err(From::from))
            },
            Err(e)        => Parser(self.0, Err(e)),
            Incomplete(i) => Parser(self.0, Incomplete(i))
        }
    }

    /// Executes ``f`` in the parser if the parser is in a success-state, throwing away current
    /// result, if any.
    ///
    /// Sugar for ``self.bind(|_, p| f(p))``.
    #[inline]
    pub fn then<F, U>(self, f: F) -> Parser<'a, I, U, E>
      where F: FnOnce(Parser<'a, I, (), E>) -> Parser<'a, I, U, E> {
        self.bind(|_, p| f(p))
    }

    #[inline]
    pub fn ret<U, O>(self, v: U) -> Parser<'a, I, U, O> {
        Parser(self.0, Ok(v))
    }

    #[inline]
    pub fn err<O, U>(self, e: O) -> Parser<'a, I, U, O> {
        Parser(self.0, Err(e))
    }
    
    /// Executes ``f`` in the parser if the parser is in a success state, ignoring the result of
    /// ``f`` as long as it is ok, propagating the original value.
    /// 
    /// Sugar for ``self.bind(|r, p| f(p).bind(|_, p| p.ret(r)))``.
    #[inline]
    pub fn skip<F, U>(self, f: F) -> Parser<'a, I, T, E>
      where F: FnOnce(Parser<'a, I, (), E>) -> Parser<'a, I, U, E> {
        self.bind(|r, p| f(p).bind(move |_, p| p.ret(r)))
    }
}

// Getters
// TODO: How to guarantee that they are not run on an error-state?
// Currently there is a bug if you do: ``parser.skip_while1(...).take_while(...)`` as the second
// parser will overwrite the state of the former, so any error in skip_while will be ignored.
impl<'a, I: 'a + Copy, E: From<Error<I>>> Parser<'a, I, (), E> {
    /// Ensures that at least ``n`` items are present and returns a slice of
    /// items from the current input.
    fn ensure(self, n: usize) -> Parser<'a, I, &'a [I], E> {
        if self.0.len() >= n {
            Parser(self.0, Ok(&self.0[..n]))
        } else {
            Parser(self.0, Incomplete(self.0.len()))
        }
    }

    pub fn peek(self) -> Parser<'a, I, I, E> {
        Parser(self.0, if self.0.len() > 0 { Ok(self.0[0]) } else { Incomplete(self.0.len()) })
    }

    pub fn get(self) -> Parser<'a, I, I, Error<I>> {
        if self.0.len() > 0 {
            let (d, r) = self.0.split_at(1);

            Parser(r, Ok(d[0]))
        } else {
            Parser(self.0, Incomplete(self.0.len()))
        }
    }

    fn advance(self, n: usize) -> Parser<'a, I, (), E> {
        Parser(&self.0[n..], self.1)
    }

    pub fn satisfy<F>(self, f: F) -> Parser<'a, I, I, E>
      where F: FnOnce(I) -> bool {
        self.peek().bind(|v, p|
            if f(v) {
                p.advance(1).ret(v)
            } else {
                p.err(Error::DidNotSatisfy(v))
            }
        )
    }

    pub fn take_while1<F>(self, f: F) -> Parser<'a, I, &'a [I], E>
      where F: Fn(I) -> bool {
        let Parser(buf, _) = self;

        match buf.iter().map(|c| *c).position(|c| f(c) == false) {
            Some(0) => Parser(buf, Err(From::from(Error::DidNotSatisfy(buf[0])))),
            Some(n) => Parser(&buf[n..], Ok(&buf[0..n])),
            None    => Parser(buf, Incomplete(buf.len())),
        }
    }

    pub fn take_while<F>(self, f: F) -> Parser<'a, I, &'a [I], E>
      where F: Fn(I) -> bool {
        let Parser(buf, _) = self;

        match buf.iter().map(|c| *c).position(|c| f(c) == false) {
            Some(n) => Parser(&buf[n..], Ok(&buf[0..n])),
            None    => Parser(buf, Incomplete(buf.len())),
        }
    }

    pub fn take_till<F>(self, f: F) -> Parser<'a, I, &'a [I], E>
      where F: Fn(I) -> bool {
        let Parser(buf, _) = self;

        match buf.iter().map(|c| *c).position(f) {
            Some(n) => Parser(&buf[n..], Ok(&buf[0..n])),
            None    => Parser(buf, Incomplete(buf.len())),
        }
    }
}

// Getters, with equality
// TODO: How to guarantee that they are not run on an error-state?
// The () bound guarantees that it most likely is not run in one since then() is almost required.
impl<'a, I: 'a + Copy + Eq, E: From<Error<I>>> Parser<'a, I, (), E> {
    pub fn char(self, c: I) -> Parser<'a, I, I, E> {
        self.satisfy(|i| i == c)
    }

    // TODO: Sequence equality
}

// Skipping and buffer modifying methods
impl<'a, I: 'a + Copy, T, E: From<Error<I>>> Parser<'a, I, T, E> {
    pub fn skip_while1<F>(self, f: F) -> Parser<'a, I, T, E>
      where F: Fn(I) -> bool {
        self.skip(|p| p.take_while1(f))
    }

    pub fn skip_while<F>(self, f: F) -> Parser<'a, I, T, E>
      where F: Fn(I) -> bool {
        self.skip(|p| p.take_while(f))
    }
}

// Combinators with state
impl<'a, I: 'a + Copy, T, E> Parser<'a, I, T, E> {
    pub fn or<F>(self, f: F) -> Self
      where F: FnOnce(Parser<'a, I, (), E>) -> Parser<'a, I, T, E> {
        match self.1 {
            Ok(_)         => self,
            Err(_)        => f(Parser(self.0, Ok(()))),
            Incomplete(_) => self,
        }
    }
}

// Combinators without initial state
// TODO: How to guarantee that they are not run on an error-state?
// The () bound guarantees that it most likely is not run in one since then() is almost required.
impl<'a, 'b, I: 'a + Copy, E> Parser<'a, I, (), E> {
    pub fn many<F, T, U>(self, f: F) -> Parser<'a, I, T, E>
      where F: FnMut(Parser<'a, I, (), E>) -> Parser<'a, I, U, E>,
            T: FromIterator<U> {
        let mut i = Iter::new(self.0, f);

        let r: T = FromIterator::from_iter(i.by_ref());

        match i.last_state() {
            // We haven't read everything yet
            Result::Good       => Parser(self.0, Incomplete(self.0.len())),
            // Ok, last parser failed, we have iterated all
            Result::Bad        => Parser(i.buffer(), Ok(r)),
            // Nested parser incomplete, propagate
            Result::Incomplete => Parser(self.0, Incomplete(self.0.len())),
        }
    }
}

use std::marker::PhantomData;

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Result {
    Good,
    Bad,
    Incomplete,
}

pub struct Iter<'a, I: 'a + Copy, U, E, F>
  where F: FnMut(Parser<'a, I, (), E>) -> Parser<'a, I, U, E> {
    /// Last resulting state
    l: Result,
    /// Current buffer
    b: &'a [I],
    f: F,
    u: PhantomData<U>,
    e: PhantomData<E>,
}

impl<'a, I: 'a + Copy, E, U, F> Iter<'a, I, U, E, F>
  where F: FnMut(Parser<'a, I, (), E>) -> Parser<'a, I, U, E> {
    pub fn new(buffer: &'a [I], f: F) -> Iter<'a, I, U, E, F> {
        Iter{
            l: Result::Good,
            b: buffer,
            f: f,
            u: PhantomData,
            e: PhantomData
        }
    }

    pub fn last_state(&self) -> Result {
        self.l
    }

    pub fn buffer(&self) -> &'a [I] {
        self.b
    }
}

impl<'a, I: 'a + Copy, E, U, F> Iterator for Iter<'a, I, U, E, F>
  where F: FnMut(Parser<'a, I, (), E>) -> Parser<'a, I, U, E> {
    type Item = U;

    fn next(&mut self) -> Option<Self::Item> {
        if self.l != Result::Good {
            return None
        }

        let Parser(b, r) = (self.f)(Parser(self.b, State::Ok(())));

        match r {
            State::Ok(v)      => {
                self.l = Result::Good;
                self.b = b;

                Some(v)
            },
            State::Err(_)        => { self.l = Result::Bad;        None },
            State::Incomplete(_) => { self.l = Result::Incomplete; None },
        }
    }
}

// State extractors
impl<'a, I: 'a + Copy, T, E: fmt::Debug> Parser<'a, I, T, E> {
    // TODO: Is this actually useful?
    pub fn unwrap(self) -> T {
        match self.1 {
            Ok(t)         => t,
            Err(e)        => panic!("Parser in error state: {:?}", e),
            Incomplete(i) => panic!("Parser is incomplete, with {} items remaining", i),
        }
    }
}

// Syntactic sugar:

/// * === fmap
impl<'a, I: 'a + Copy, T, E, U, F> ops::Mul<F> for Parser<'a, I, T, E>
  where F: FnOnce(T) -> U {
    type Output = Parser<'a, I, U, E>;
    
    fn mul(self, rhs: F) -> Self::Output {
        self.map(rhs)
    }
}

/// << === bind
impl<'a, I: 'a + Copy, T, E, U, F> ops::Shl<F> for Parser<'a, I, T, E>
  where F: FnOnce(T, Parser<'a, I, (), E>) -> Parser<'a, I, U, E> {
    type Output = Parser<'a, I, U, E>;

    fn shl(self, rhs: F) -> Self::Output {
        self.bind(rhs)
    }
}

/// >> === then
impl<'a, I: 'a + Copy, T, E, U, F> ops::Shr<F> for Parser<'a, I, T, E>
  where F: FnOnce(Parser<'a, I, (), E>) -> Parser<'a, I, U, E> {
    type Output = Parser<'a, I, U, E>;

    fn shr(self, rhs: F) -> Self::Output {
        self.then(rhs)
    }
}

/// | === or
impl<'a, I: 'a + Copy, T, E, F> ops::BitOr<F> for Parser<'a, I, T, E>
  where F: FnOnce(Parser<'a, I, (), E>) -> Parser<'a, I, T, E> {
    type Output = Parser<'a, I, T, E>;

    fn bitor(self, rhs: F) -> Self::Output {
        self.or(rhs)
    }
}

pub fn from_slice<'a, I: 'a + Copy>(s: &'a [I]) -> Parser<'a, I, (), Error<I>> {
    Parser(s, Ok(()))
}

impl<'a, I: 'a + Copy, E> From<&'a [I]> for Parser<'a, I, (), E> {
    /// Creates a parser from a slice.
    fn from(s: &'a [I]) -> Parser<'a, I, (), E> {
        Parser(s, Ok(()))
    }
}

#[cfg(test)]
mod test {
    use super::*;

    fn ascii<'a>(p: Parser<'a, u8, (), Error<u8>>) -> Parser<'a, u8, u32, Error<u8>> {
        p.satisfy(|c| c < 128).map(|c| c as u32)
    }

    fn trailing<'a>(a: u32, p: Parser<'a, u8, (), Error<u8>>) -> Parser<'a, u8, u32, Error<u8>> {
        p.satisfy(|c| c & 0b11000000 == 0b10000000).map(|c| (a << 6) + (c & 0b00111111) as u32)
    }

    fn twobyte<'a>(p: Parser<'a, u8, (), Error<u8>>) -> Parser<'a, u8, u32, Error<u8>> {
        p.satisfy(|c| c & 0b11100000 == 0b11000000) * (|c| (c & 0b00011111) as u32) << trailing
        /*p.satisfy(|c| (c & 0b11100000) == 0b11000000).map(|c| (c & 0b000111111) as u32)
         .bind(trailing)*/
    }

    fn threebyte<'a>(p: Parser<'a, u8, (), Error<u8>>) -> Parser<'a, u8, u32, Error<u8>> {
        p.satisfy(|c| c & 0b11110000 == 0b1110000).map(|c| (c & 0b000111111) as u32)
         .bind(trailing)
         .bind(trailing)
    }

    fn parse_utf8<'a>(p: Parser<'a, u8, (), Error<u8>>) -> Parser<'a, u8, u32, Error<u8>> {
        // p >> ascii | twobyte | threebyte
        p.then(ascii)
         .or(twobyte)
         .or(threebyte)
    }

    fn parse_utf8_inline<'a>(p: Parser<'a, u8, (), Error<u8>>) -> Parser<'a, u8, u32, Error<u8>> {
        p.satisfy(|c| c < 128).map(|c| c as u32)
            .or(|p|
                p.satisfy(|c| c & 0b11100000 == 0b11000000).map(|c| (c & 0b00011111) as u32).bind(|a, p|
                    p.satisfy(|c| c & 0b11000000 == 0b10000000).map(|c| (a << 6) + (c & 0b00111111) as u32)))
            .or(|p|
                p.satisfy(|c| c & 0b11110000 == 0b1110000).map(|c| (c & 0b000111111) as u32).bind(|a, p|
                    p.satisfy(|c| c & 0b11000000 == 0b10000000).map(|c| (a << 6) + (c & 0b00111111) as u32)).bind(|a, p|
                        p.satisfy(|c| c & 0b11000000 == 0b10000000).map(|c| (a << 6) + (c & 0b00111111) as u32)))
        /*let ascii = |p|
            p.satisfy(|c| c < 128)
             .map(|c| c as u32);
        let tail = |a, p|
            p.satisfy(|c| c & 0b11000000 == 0b10000000)
             .map(|c| (a << 6) + (c & 0b00111111) as u32);
        let twobyte = |p|
            p.satisfy(|c| (c & 0b11100000) == 0b11000000)
             .map(|c| (c & 0b000111111) as u32)
             .bind(tail);
        let threebyte = |p|
            p.satisfy(|c| c & 0b11110000 == 0b1110000)
             .map(|c| (c & 0b000111111) as u32)
             .bind(tail)
             .bind(tail);

        ascii(p)
         .or(twobyte)
         .or(threebyte)*/
    }

    #[test]
    fn test_utf8() {
        let data = "ä-123".as_bytes();

        let p = Parser::new(data);

        assert_eq!(parse_utf8(p).unwrap(), 0xe4);
    }
}
