use Parser;
use parser;
use State;
use error;
use error::Error;

/// Matches any item, returning it if present.
///
/// If the buffer length is 0 this parser is considered incomplete.
///
/// ```
/// use parser::{Parser, any};
///
/// let p = any(p);
///
/// assert_eq!(p.parse(b"abc").unwrap(), b'a');
/// ```
#[inline]
pub fn any<'a, I, E = Error<I>>() -> impl Parser<'a, I, I, E>
  where I: Copy {
    parser(move |i| match i.first() {
        Some(&c) => State::Item(&i[1..], c),
        None     => State::Incomplete(1),
    })
}

/// Matches a single character, returning the matched character on success.
///
/// If the buffer length is 0 this parser is considered incomplete.
///
/// ```
/// use parser::{Error, Parser, char};
///
/// let p = char(p, b'a');
///
/// assert_eq!(p.parse(b"abc").unwrap(), b'a');
/// ```
#[inline]
pub fn char<'a, 'p, I>(chr: I) -> impl Parser<'a, I, I, Error<I>> + 'p
  where I: Copy + Eq + 'p {
    parser(move |i| match i.first() {
        Some(&c) if chr == c => State::Item(&i[1..], c),
        Some(_)              => State::Error(i, error::expected(chr)),
        None                 => State::Incomplete(1),
    })
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
pub fn satisfy<'a, 'p, I, F>(f: F) -> impl Parser<'a, I, I, Error<I>> + 'p
  where I: Copy,
        F: FnOnce(I) -> bool + 'p {
    parser(move |i| match i.first() {
        Some(&c) if f(c) => State::Item(&i[1..], c),
        Some(_)          => State::Error(i, error::unexpected()),
        None             => State::Incomplete(1),
    })
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
pub fn take_while<'a, 'p, I, F>(f: F) -> impl Parser<'a, I, &'a [I], Error<I>> + 'p
  where I: Copy + 'a,
        F: Fn(I) -> bool + 'p {
    parser(move |i| match i.iter().position(|&c| f(c) == false) {
        Some(n) => State::Item(&i[n..], &i[..n]),
        // TODO: Should this following 1 be something else, seeing as take_while1 is potentially
        // infinite?
        None    => State::Incomplete(1),
    })
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
pub fn take_while1<'a, 'p, I, F>(f: F) -> impl Parser<'a, I, &'a [I], Error<I>> + 'p
  where I: Copy + 'a,
        F: Fn(I) -> bool + 'p {
    parser(move |i| match i.iter().position(|&c| f(c) == false) {
        Some(0) => State::Error(i, error::unexpected()),
        Some(n) => State::Item(&i[n..], &i[..n]),
        // TODO: Should this following 1 be something else, seeing as take_while1 is potentially
        // infinite?
        None    => State::Incomplete(1),
    })
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
pub fn take_till<'a, 'p, I, F>(f: F) -> impl Parser<'a, I, &'a [I], Error<I>> + 'p
  where I: Copy + 'a,
        F: Fn(I) -> bool + 'p {
    parser(move |i| match i.iter().map(|c| *c).position(f) {
        Some(n) => State::Item(&i[n..], &i[..n]),
        // TODO: Should this following 1 be something else, seeing as take_while1 is potentially
        // infinite?
        None    => State::Incomplete(1),
    })
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
#[inline]
pub fn string<'a, 'p, I>(s: &'p [I]) -> impl Parser<'a, I, &'a [I], Error<I>> + 'p
  where I: Copy + Eq + 'a {
    parser(move |i| {
        if s.len() > i.len() {
            return State::Incomplete(s.len() - i.len());
        }

        let d = &i[..s.len()];

        for j in 0..s.len() {
            if s[j] != d[j] {
                return error::string::<I, &[I]>(i, j, s);
            }
        }

        State::Item(&i[s.len()..], d)
    })
}
