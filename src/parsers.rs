use ::{
    Empty,
    Parser,
    State,
};

use std::fmt;
use std::error;
use std::any;

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Error<T: Copy> {
    /// Did not expect the given token.
    Unexpected(T),
    /// Expected the first token, got the second token instead.
    Expected(T, T),
    /// Expected anything but ``T``.
    NotExpect(T),
}

impl<T: fmt::Debug + Copy> fmt::Display for Error<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            &Error::Unexpected(t)  => write!(f, "unexpected '{:?}' while parsing", t),
            &Error::Expected(e, a) => write!(f, "expected '{:?}', got '{:?}'", e, a),
            &Error::NotExpect(e)   => write!(f, "expected anything but '{:?}', got {:?}", e, e),
        }
    }
}

impl<T: any::Any + fmt::Debug + Copy> error::Error for Error<T> {
    fn description(&self) -> &str {
        match self {
            &Error::Unexpected(_)  => "An unexpected character was encountered",
            &Error::Expected(_, _) => "Expected a certain character, got another",
            &Error::NotExpect(_)   => "Expected any character but one, got the one",
        }
    }
}

/// Matches any item, returning it if present.
///
/// If the buffer length is 0 this parser is considered incomplete.
///
/// ```
/// use parser::{Error, Parser, any};
///
/// let p: Parser<_, _, _> = From::from(b"abc" as &[u8]);
///
/// assert_eq!(any(p).unwrap(), b'a');
/// ```
#[inline]
pub fn any<'a, I: 'a + Copy>(m: Empty<'a, I>) -> Parser<'a, I, I, Error<I>> {
    match m.0.first() {
        Some(&c) => Parser(&m.0[1..], State::Ok(c)),
        None     => Parser(m.0,       State::Incomplete(m.0, 1)),
    }
}

/// Matches an item as long as it is not equal to ``c``.
/// 
/// If the buffer length is 0 this parser is considered incomplete.
/// 
/// ```
/// use parser::{Error, Parser, not_char};
/// 
/// let p: Parser<_, _, _> = From::from(b"abc");
/// 
/// assert_eq!(not_char(p, b'c').unwrap(), b'a');
/// ```
#[inline]
pub fn not_char<'a, I: 'a + Copy + Eq>(m: Empty<'a, I>, c: I) -> Parser<'a, I, I, Error<I>> {
    match m.0.first() {
        Some(&i) if i != c => Parser(&m.0[1..], State::Ok(i)),
        Some(&i)           => Parser(m.0,       State::Err(m.0, Error::NotExpect(i))),
        None               => Parser(m.0,       State::Incomplete(m.0, 1)),
    }
}

/// Matches a single character, returning the matched character on success.
///
/// If the buffer length is 0 this parser is considered incomplete.
///
/// ```
/// use parser::{Error, Parser, char};
///
/// let p: Parser<_, _, _> = From::from(b"abc" as &[u8]);
///
/// assert_eq!(char(p, b'a').unwrap(), b'a');
/// ```
#[inline]
pub fn char<'a, I: 'a + Copy + Eq>(m: Empty<'a, I>, c: I) -> Parser<'a, I, I, Error<I>> {
    match m.0.first().map(|i| *i) {
        None              => Parser(m.0,       State::Incomplete(m.0, 1)),
        Some(i) if i == c => Parser(&m.0[1..], State::Ok(c)),
        Some(i)           => Parser(m.0,       State::Err(m.0, Error::Expected(c, i))),
    }
}

/// Matches ``num`` items no matter what they are, returning a slice of the matched items.
/// 
/// If the buffer length is less than ``num`` this parser is considered incomplete.
/// 
/// ```
/// use parser::{Parser, take};
/// 
/// let p = From::from(b"abcd");
/// 
/// assert_eq!(take(p, 3).unwrap(), b"abc");
/// ```
#[inline]
pub fn take<'a, I: 'a + Copy>(m: Empty<'a, I>, num: usize) -> Parser<'a, I, &'a [I], Error<I>> {
    if num <= m.0.len() {
        Parser(&m.0[num..], State::Ok(&m.0[..num]))
    } else {
        Parser(m.0, State::Incomplete(m.0, num))
    }
}

/// Matches all items until ``f`` returns false, if at least one item matched this parser succeeds
/// and returns a slice of all the matched items.
/// 
/// If no failure can be found the parser will be considered to be incomplete as there might be
/// more input which needs to be matched. If zero items were matched an error will be returned.
///
/// ```
/// use parser::{Error, Parser, take_while1};
///
/// let p: Parser<_, _, _> = From::from(b"abcdcba" as &[u8]);
///
/// assert_eq!(take_while1(p, |c| c == b'a' || c == b'b').unwrap(), b"ab");
/// ```
#[inline]
pub fn take_while1<'a, I: 'a + Copy, F>(m: Empty<'a, I>, f: F) -> Parser<'a, I, &'a [I], Error<I>>
  where F: Fn(I) -> bool {
    let Parser(buf, _) = m;

    match buf.iter().map(|c| *c).position(|c| f(c) == false) {
        Some(0) => Parser(buf,       State::Err(buf, Error::Unexpected(buf[0]))),
        Some(n) => Parser(&buf[n..], State::Ok(&buf[0..n])),
        // TODO: Should this following 1 be something else, seeing as take_while1 is potentially
        // infinite?
        None    => Parser(buf,       State::Incomplete(buf, 1)),
    }
}

#[cfg(test)]
mod test {
    use ::{
        Parser,
        State,
        bind,
        ret,
    };
    use super::*;

    #[test]
    fn test_char() {
        let b = "ab".as_bytes();

        let m: Parser<_, _, _> = From::from(b);

        let Parser(buf, r) = char(m, b'a');

        assert_eq!(buf, b"b");
        assert_eq!(r, State::Ok(b'a'));
    }

    #[test]
    fn test_char_fail() {
        let b = "ab".as_bytes();

        let m: Parser<_, _, _> = From::from(b);

        let Parser(buf, r) = char(m, b'b');

        assert_eq!(buf, b"ab");
        assert_eq!(r, State::Err(b"ab", Error::Expected(b'b', b'a')));
    }

    #[test]
    fn test_char_empty() {
        let b = "".as_bytes();

        let m: Parser<_, _, _> = From::from(b);

        let Parser(buf, r) = char(m, b'b');

        assert_eq!(buf, b"");
        assert_eq!(r, State::Incomplete(b"", 1));
    }

    #[test]
    fn parse_decimal() {
        fn is_digit(c: u8) -> bool {
            c >= b'0' && c <= b'9'
        }

        fn decimal<'a>(m: Parser<'a, u8, (), ()>) -> Parser<'a, u8, usize, Error<u8>> {
            /*bytes <- take_while1 is_digit
            ret bytes.iter().fold(0, |a, b| a * 10 + (b - b'0') as usize)

            take_while1 is_digit >>= \bytes ->
                ret bytes.iter().fold(0, |a, b| a * 10 + (b - b'0') as usize)

            bind(take_while1(m, is_digit), |bytes|
                return bytes.iter().fold(0, |a, b| a * 10 + (b - b'0') as usize))*/

            bind(take_while1(m, is_digit), |m, bytes|
                ret(m, bytes.iter().fold(0, |a, b| a * 10 + (b - b'0') as usize)))
        }

        /*let f_num = mdo!(
            real <- decimal
            b'.'
            frac <- decimal
            ret (real, frac)
        );

        decimal >>= \real ->
            b'.' >>= \_ ->
                decimal >>= (\frac ->
                    return (real, frac)

        bind(decimal, |real|
            bind(b'.', |_|
                bind(decimal, |frac|
                    return (real, frac))))*/

        let b = "123.4567 ".as_bytes();

        let m: Parser<_, _, _> = From::from(b);

        let Parser(buf, state) =
            bind(decimal(m), |m, real|
                bind(char(m, b'.'), |m, _| {
                    bind(decimal(m), |m, frac|
                        ret::<_, _, Error<u8>>(m, (real, frac)))}));

        assert_eq!(buf, &[b' ']);
        assert_eq!(state, State::Ok((123, 4567)));
    }
}
