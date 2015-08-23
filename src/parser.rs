use Parser;
use State;
use Input;
use error;
use error::Error;

/// Matches any item, returning it if present.
///
/// If the buffer length is 0 this parser is considered incomplete.
///
/// ```
/// use parser::{Parser, any};
///
/// let p = From::from(b"abc");
///
/// assert_eq!(any(p).unwrap(), b'a');
/// ```
#[inline]
pub fn any<'a, I>(m: Input<'a, I>) -> Parser<'a, I, I, Error<I>>
  where I: Copy {
    satisfy(m, |_| true)
}

/// Matches a single character, returning the matched character on success.
///
/// If the buffer length is 0 this parser is considered incomplete.
///
/// ```
/// use parser::{Error, Parser, char};
///
/// let p = From::from(b"abc");
///
/// assert_eq!(char(p, b'a').unwrap(), b'a');
/// ```
#[inline]
pub fn char<'a, I>(m: Input<'a, I>, chr: I) -> Parser<'a, I, I, Error<I>>
  where I: Copy + Eq {
    match m.0.first() {
        Some(&c) if chr == c => Parser(State::Item(&m.0[1..], c)),
        Some(_)              => Parser(State::Error(m.0, error::expected(chr))),
        None                 => Parser(State::Incomplete(1)),
    }
}

/// Matches an item using ``f``, the item is returned if ``f`` yields true, otherwise this parser
/// fails.
///
/// If the buffer length is 0 this parser is considered incomplete.
/// 
/// ```
/// use parser::{Parser, satisfy};
/// 
/// let p = From::from(b"abc");
/// 
/// assert_eq!(satisfy(p, |c| c == b'a').unwrap(), b'a');
/// ```
#[inline]
pub fn satisfy<'a, I, F>(m: Input<'a, I>, f: F) -> Parser<'a, I, I, Error<I>>
  where I: Copy,
        F: FnOnce(I) -> bool {
    match m.0.first() {
        Some(&c) if f(c) => Parser(State::Item(&m.0[1..], c)),
        Some(_)          => Parser(State::Error(m.0, Error::Unexpected)),
        None             => Parser(State::Incomplete(1)),
    }
}

/// Matches all items while ``f`` returns false, returns a slice of all the matched items.
/// 
/// If no failure can be found the parser will be considered to be incomplete as there might be
/// more input which needs to be matched.
/// 
/// ```
/// use parser::{Parser, take_while};
///
/// let p = From::from(b"abcdcba");
///
/// assert_eq!(take_while(p, |c| c == b'a' || c == b'b').unwrap(), b"ab");
/// ```
/// 
/// Without managing to match anything:
/// 
/// ```
/// use parser::{Parser, take_while};
///
/// let p = From::from(b"abcdcba");
///
/// assert_eq!(take_while(p, |c| c == b'z').unwrap(), b"");
/// ```
#[inline]
pub fn take_while<'a, I, F>(m: Input<'a, I>, f: F) -> Parser<'a, I, &'a [I], Error<I>>
  where I: Copy,
        F: Fn(I) -> bool {
    match m.0.iter().position(|&c| f(c) == false) {
        Some(n) => Parser(State::Item(&m.0[n..], &m.0[..n])),
        // TODO: Should this following 1 be something else, seeing as take_while1 is potentially
        // infinite?
        None    => Parser(State::Incomplete(1)),
    }
}

/// Matches all items while ``f`` returns true, if at least one item matched this parser succeeds
/// and returns a slice of all the matched items.
/// 
/// If no failure can be found the parser will be considered to be incomplete as there might be
/// more input which needs to be matched. If zero items were matched an error will be returned.
///
/// ```
/// use parser::{Parser, take_while1};
///
/// let p = From::from(b"abcdcba");
///
/// assert_eq!(take_while1(p, |c| c == b'a' || c == b'b').unwrap(), b"ab");
/// ```
#[inline]
pub fn take_while1<'a, I, F>(m: Input<'a, I>, f: F) -> Parser<'a, I, &'a [I], Error<I>>
  where I: Copy,
        F: Fn(I) -> bool {
    match m.0.iter().position(|&c| f(c) == false) {
        Some(0) => Parser(State::Error(m.0, Error::Unexpected)),
        Some(n) => Parser(State::Item(&m.0[n..], &m.0[..n])),
        // TODO: Should this following 1 be something else, seeing as take_while1 is potentially
        // infinite?
        None    => Parser(State::Incomplete(1)),
    }
}

/// Matches all items until ``f`` returns true, all items to that point will be returned as a slice
/// upon success.
/// 
/// If no failure can be found the parser will be considered to be incomplete as there might be
/// more input which needs to be matched.
/// 
/// ```
/// use parser::{Parser, take_till};
/// 
/// let p = From::from(b"abcdef");
/// 
/// assert_eq!(take_till(p, |c| c == b'd').unwrap(), b"abc");
/// ```
#[inline]
pub fn take_till<'a, I, F>(m: Input<'a, I>, f: F) -> Parser<'a, I, &'a [I], Error<I>>
  where I: Copy,
        F: Fn(I) -> bool {
    match m.0.iter().map(|c| *c).position(f) {
        Some(n) => Parser(State::Item(&m.0[n..], &m.0[..n])),
        // TODO: Should this following 1 be something else, seeing as take_while1 is potentially
        // infinite?
        None    => Parser(State::Incomplete(1)),
    }
}

/// Matches the given slice against the parser, returning the matched slice upon success.
/// 
/// If the length of the contained data is shorter than the given slice this parser is considered
/// incomplete.
/// 
/// ```
/// use parser::{Parser, string};
/// 
/// let p = From::from(b"abcdef");
/// 
/// assert_eq!(string(p, b"abc").unwrap(), b"abc");
/// ```
#[cfg(feature = "verbose_error")]
#[inline]
pub fn string<'a, I>(m: Input<'a, I>, s: &[I]) -> Parser<'a, I, &'a [I], Error<I>>
  where I: Copy + Eq {
    if s.len() > m.0.len() {
        return Parser(State::Incomplete(s.len() - m.0.len()));
    }

    let d = &m.0[..s.len()];

    for i in 0..s.len() {
        if s[i] != d[i] {
            return Parser(State::Error(m.0, error::string(s)));
        }
    }

    Parser(State::Item(&m.0[s.len()..], d))
}

/// Matches the given slice against the parser, returning the matched slice upon success.
/// 
/// If the length of the contained data is shorter than the given slice this parser is considered
/// incomplete.
/// 
/// ```
/// use parser::{Parser, string};
/// 
/// let p = From::from(b"abcdef");
/// 
/// assert_eq!(string(p, b"abc").unwrap(), b"abc");
/// ```
#[cfg(not(feature = "verbose_error"))]
#[inline]
pub fn string<'a, I>(m: Input<'a, I>, s: &[I]) -> Parser<'a, I, &'a [I], Error<I>>
  where I: Copy + Eq {
    if s.len() > m.0.len() {
        return Parser(State::Incomplete(s.len() - m.0.len()));
    }

    let d = &m.0[..s.len()];

    for i in 0..s.len() {
        if s[i] != d[i] {
            return Parser(State::Error(&m.0[i..], Error::Unexpected));
        }
    }

    Parser(State::Item(&m.0[s.len()..], d))
}
