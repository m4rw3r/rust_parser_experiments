use std::iter::FromIterator;

#[cfg(not(feature = "verbose_error"))]
use error;

use Input;
use Parser;
use State;

#[cfg(not(feature = "verbose_error"))]
use Error;

use iter::Iter;
use iter::IResult;

/// Tries to match the parser ``f``, if ``f`` fails it tries ``g``. Returns the success value of
/// the first match, otherwise the error of the last one if both fail.
/// 
/// Incomplete state is propagated from the first one to report incomplete.
/// 
/// ```
/// use parser::{Parser, or, char};
/// 
/// let p = From::from(b"abc");
/// 
/// assert_eq!(or(p, |m| char(m, b'b'), |m| char(m, b'a')).unwrap(), b'a');
/// ```
#[inline]
pub fn or<'a, I, T, E, F, G>(m: Input<'a, I>, f: F, g: G) -> Parser<'a, I, T, E>
  where I: Copy,
        F: Fn(Input<'a, I>) -> Parser<'a, I, T, E>,
        G: Fn(Input<'a, I>) -> Parser<'a, I, T, E> {
    let buf = m.0;

    match f(Input(buf)).0 {
        State::Item(b, t)    => Parser(State::Item(b, t)),
        State::Error(_, _)   => g(Input(buf)),
        State::Incomplete(r) => Parser(State::Incomplete(r)),
    }
}

/// Parses many instances of ``f`` until it does no longer match, returning all matches.
/// 
/// Note: If the last parser succeeds on the last input item then this parser is still considered
/// incomplete as there might be more data to fill.
/// 
/// Note: Allocates data.
/// 
/// ```
/// use parser::{Error, Parser, bind, char, many, ret, take_while1};
/// 
/// let p = From::from(b"a,bc,cd ");
/// 
/// let r: Parser<_, Vec<&[u8]>, Error<_>> = many(p, |m| bind(
///     take_while1(m, |c| c != b',' && c != b' '), |m, c| bind(
///         char(m, b','), |m, _| ret(m, c))));
/// let v = r.unwrap();
///
/// assert_eq!(v.len(), 2);
/// assert_eq!(v[0], b"a");
/// assert_eq!(v[1], b"bc");
/// ```
#[inline]
pub fn many<'a, I, T, E, F, U>(m: Input<'a, I>, f: F) -> Parser<'a, I, T, E>
  where I: Copy,
        F: Fn(Input<'a, I>) -> Parser<'a, I, U, E>,
        T: FromIterator<U> {
    let mut iter = Iter::new(m.0, f);

    let result: T = FromIterator::from_iter(iter.by_ref());

    match iter.last_state() {
        // We haven't read everything yet
        IResult::Good       => Parser(State::Incomplete(1)),
        // Ok, last parser failed, we have iterated all.
        // Return remainder of buffer and the collected result
        IResult::Bad        => Parser(State::Item(iter.buffer(), result)),
        // Nested parser incomplete, propagate
        IResult::Incomplete => Parser(State::Incomplete(1)),
    }
}

/// Parses at least one instance of ``f`` and continues until it does no longer match,
/// returning all matches.
/// 
/// Note: If the last parser succeeds on the last input item then this parser is still considered
/// incomplete as there might be more data to fill.
/// 
/// Note: Allocates data.
/// 
/// ```should_panic
/// use parser::{Error, Parser, bind, char, many1, ret, take_while1};
///
/// let p = From::from(b"a");
///
/// let r: Parser<_, Vec<&[u8]>, Error<_>> = many1(p, |m| bind(
///     take_while1(m, |c| c != b',' && c != b' '), |m, c| bind(
///         char(m, b','), |m, _| ret::<_, _, Error<_>>(m, c))));
///
/// let _ = r.unwrap();
/// ```
// TODO: Proper checking of return value in doc-test
#[cfg(not(feature = "verbose_error"))]
#[inline]
pub fn many1<'a, I, T, E, F, U>(m: Input<'a, I>, f: F) -> Parser<'a, I, T, Error<I>>
  where I: Copy,
        F: Fn(Input<'a, I>) -> Parser<'a, I, U, E>,
        T: FromIterator<U> {
    // If we have gotten an item, if this is false after from_iter we have failed
    let mut item = false;
    let mut iter = Iter::new(m.0, f);

    let result: T = FromIterator::from_iter(iter.by_ref().inspect(|_| item = true ));

    match (item, iter.last_state()) {
        // We haven't read everything yet
        (true,  IResult::Good)       => Parser(State::Incomplete(1)),
        // Ok, last parser failed, we have iterated all
        (true,  IResult::Bad)        => Parser(State::Item(iter.buffer(), result)),
        // Nested parser incomplete, propagate
        (_,     IResult::Incomplete) => Parser(State::Incomplete(1)),
        (false, _)                   => Parser(State::Error(m.0, error::many1())),
    }
}

/// Parses at least one instance of ``f`` and continues until it does no longer match,
/// returning all matches.
/// 
/// Note: If the last parser succeeds on the last input item then this parser is still considered
/// incomplete as there might be more data to fill.
/// 
/// Note: Allocates data.
/// 
/// ```should_panic
/// use parser::{Error, Parser, bind, char, many1, ret, take_while1};
///
/// let p = From::from(b"a");
///
/// let r: Parser<_, Vec<&[u8]>, Error<_>> = many1(p, |m| bind(
///     take_while1(m, |c| c != b',' && c != b' '), |m, c| bind(
///         char(m, b','), |m, _| ret::<_, _, Error<_>>(m, c))));
///
/// let _ = r.unwrap();
/// ```
// TODO: Proper checking of return value in doc-test
#[cfg(feature = "verbose_error")]
#[inline]
pub fn many1<'a, I, T, E, F, U>(m: Input<'a, I>, f: F) -> Parser<'a, I, T, E>
  where I: Copy,
        F: Fn(Input<'a, I>) -> Parser<'a, I, U, E>,
        T: FromIterator<U> {
    // If we have gotten an item, if this is false after from_iter we have failed
    let mut item = false;
    let mut iter = Iter::new(m.0, f);

    let result: T = FromIterator::from_iter(iter.by_ref().inspect(|_| item = true ));

    match (item, iter.last_state()) {
        // We haven't read everything yet
        (true,  IResult::Good)       => Parser(State::Incomplete(1)),
        // Ok, last parser failed, we have iterated all
        (true,  IResult::Bad)        => Parser(State::Item(iter.buffer(), result)),
        // First parse failed, propagate error
        (false, IResult::Bad)        => Parser(State::Error(iter.buffer(), iter.error().unwrap())),
        // Should not be possible as long as next() is called
        (false, IResult::Good)       => unreachable!(),
        // Nested parser incomplete, propagate
        (_,     IResult::Incomplete) => Parser(State::Incomplete(1)),
    }
}
