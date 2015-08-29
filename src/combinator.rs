use std::iter::FromIterator;

use Input;
use Parser;
use State;

use iter::{
    Iter,
    EndState,
};

/// Applies the parser ``p`` exactly ``num`` times, propagating any error or incomplete state.
/// 
/// ```
/// use parser::{Parser, count, char};
/// 
/// let p = From::from(b"aaa");
/// 
/// let r: Vec<u8> = count(p, 3, |m| char(m, b'a')).unwrap();
/// 
/// assert_eq!(r, [b'a', b'a', b'a']);
/// ```
#[inline]
pub fn count<'a, I, T, E, F, U>(m: Input<'a, I>, num: usize, p: F) -> Parser<'a, I, T, E>
  where I: Copy,
        F: Fn(Input<'a, I>) -> Parser<'a, I, U, E>,
        T: FromIterator<U> {
    // If we have gotten an item, if this is false after from_iter we have failed
    let mut count = 0;
    let mut iter  = Iter::new(m.0, p);

    let result: T      = FromIterator::from_iter(iter.by_ref().take(num).inspect(|_| count = count + 1 ));
    let (buffer, last) = iter.end_state();

    if count == num {
        Parser(State::Item(buffer, result))
    } else {
        // Can only be less than num here since take() limits it.
        // Just propagate the last state from the iterator.
        match last {
            EndState::Incomplete(n) => Parser(State::Incomplete(n)),
            EndState::Error(b, e)   => Parser(State::Error(b, e)),
        }
    }
}

/// Tries the parser ``f``, on success it yields the parsed value, on failure ``default`` will be
/// yielded instead.
/// 
/// Incomplete state is propagated.
/// 
/// ```
/// use parser::{Input, Parser, option, char};
/// 
/// let p1: Input<u8> = From::from(b"abcd");
/// let p2: Input<u8> = From::from(b"abcd");
/// 
/// assert_eq!(option(p1, |m| char(m, b'c'), b'z').unwrap(), b'z');
/// assert_eq!(option(p2, |m| char(m, b'a'), b'z').unwrap(), b'a');
/// ```
#[inline]
pub fn option<'a, I, T, E, F>(m: Input<'a, I>, f: F, default: T) -> Parser<'a, I, T, E>
  where I: 'a + Copy,
        F: FnOnce(Input<'a, I>) -> Parser<'a, I, T, E> {
    let buf = m.0;

    match f(Input(buf)).0 {
        State::Item(b, t)    => Parser(State::Item(b, t)),
        State::Error(_, _)   => Parser(State::Item(buf, default)),
        State::Incomplete(n) => Parser(State::Incomplete(n)),
    }
}

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
