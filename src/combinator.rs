use std::iter::FromIterator;

use Input;
use Parser;
use State;

use iter::{
    Iter,
    EndState,
};

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

    match iter.end_state() {
        // Ok, last parser failed, we have iterated all.
        // Return remainder of buffer and the collected result
        (b, EndState::Error(_, _))   => Parser(State::Item(b, result)),
        // Nested parser incomplete, propagate
        (_, EndState::Incomplete(n)) => Parser(State::Incomplete(n)),
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
/// let v = r.unwrap();
/// ```
// TODO: Proper checking of return value in doc-test
#[inline]
pub fn many1<'a, I, T, E, F, U>(m: Input<'a, I>, f: F) -> Parser<'a, I, T, E>
  where I: Copy,
        F: Fn(Input<'a, I>) -> Parser<'a, I, U, E>,
        T: FromIterator<U> {
    // If we managed to parse anything
    let mut item = false;
    // If we have gotten an item, if this is false after from_iter we have failed
    let mut iter = Iter::new(m.0, f);

    let result: T = FromIterator::from_iter(iter.by_ref().inspect(|_| item = true ));

    if !item {
        match iter.end_state() {
            (_, EndState::Error(b, e))   => Parser(State::Error(b, e)),
            (_, EndState::Incomplete(n)) => Parser(State::Incomplete(n)),
        }
    } else {
        match iter.end_state() {
            (b, EndState::Error(_, _))   => Parser(State::Item(b, result)),
            // TODO: Indicate potentially more than 1?
            (_, EndState::Incomplete(n)) => Parser(State::Incomplete(n)),
        }
    }
}
