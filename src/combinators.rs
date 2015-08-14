use std::iter::FromIterator;

use ::{
    Empty,
    Error,
    Parser,
    State,
};

use ::iter::{
    IResult,
    Iter
};

/// Tries the parser ``f``, on success it yields the parsed value, on failure ``default`` will be
/// yielded instead.
/// 
/// Incomplete state is propagated.
/// 
/// ```
/// use parser::{Parser, try, char};
/// 
/// let p1 = From::from(b"abcd");
/// let p2 = From::from(b"abcd");
/// 
/// assert_eq!(try(char(p1, b'c'), b'z').unwrap(), b'z');
/// assert_eq!(try(char(p2, b'a'), b'z').unwrap(), b'a');
/// ```
#[inline]
pub fn try<'a, I: 'a + Copy, T, E>(m: Parser<'a, I, T, E>, default: T) -> Parser<'a, I, T, E> {
    match m.1 {
        State::Ok(t)            => Parser(m.0, State::Ok(t)),
        // Failure, keep using the buffer, as it points on the next item even if that item failed
        // to parse with the previous parser:
        State::Err(_, _)        => Parser(m.0, State::Ok(default)),
        State::Incomplete(b, r) => Parser(m.0, State::Incomplete(b, r)),
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
/// assert_eq!(or(char(p, b'b'), |m| char(m, b'a')).unwrap(), b'a');
/// ```
pub fn or<'a, I: 'a + Copy, T, E, F>(m: Parser<'a, I, T, E>, f: F) -> Parser<'a, I, T, E>
  where F: Fn(Empty<'a, I>) -> Parser<'a, I, T, E> {
    match m.1 {
        State::Ok(t)            => Parser(m.0, State::Ok(t)),
        State::Err(_, _)        => f(Parser(m.0, State::Ok(()))),
        State::Incomplete(b, r) => Parser(m.0, State::Incomplete(b, r)),
    }
}

/// Parses many instances of ``f`` until it does no longer match, returning all matches.
/// 
/// Note: If the last parser succeeds on the last input item then this parser is still considered
/// incomplete as there might be more data to fill.
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
pub fn many<'a, I: 'a + Copy, T, E, F, U>(m: Empty<'a, I>, f: F) -> Parser<'a, I, T, E>
  where F: Fn(Empty<'a, I>) -> Parser<'a, I, U, E>,
        T: FromIterator<U> {
    let mut iter = Iter::new(m.0, f);

    let result: T = FromIterator::from_iter(iter.by_ref());

    match iter.last_state() {
        // We haven't read everything yet
        IResult::Good       => Parser(m.0, State::Incomplete(m.0, 1)),
        // Ok, last parser failed, we have iterated all
        IResult::Bad        => Parser(iter.buffer(), State::Ok(result)),
        // Nested parser incomplete, propagate
        IResult::Incomplete => Parser(m.0, State::Incomplete(m.0, 1)),
    }
}

/// Parses at least one instance of ``f`` and continues until it does no longer match,
/// returning all matches.
/// 
/// Note: If the last parser succeeds on the last input item then this parser is still considered
/// incomplete as there might be more data to fill.
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
pub fn many1<'a, I: 'a + Copy, T, E, F, U>(m: Empty<'a, I>, f: F) -> Parser<'a, I, T, Error<I>>
  where F: Fn(Empty<'a, I>) -> Parser<'a, I, U, E>,
        T: FromIterator<U> {
    // If we have gotten an item, if this is false after from_iter we have failed
    let mut item = false;
    let mut iter = Iter::new(m.0, f);

    let result: T = FromIterator::from_iter(iter.by_ref().inspect(|_| item = true ));

    match (item, iter.last_state()) {
        // We haven't read everything yet
        (true,  IResult::Good)       => Parser(m.0, State::Incomplete(m.0, 1)),
        // Ok, last parser failed, we have iterated all
        (true,  IResult::Bad)        => Parser(iter.buffer(), State::Ok(result)),
        // Nested parser incomplete, propagate
        (_,     IResult::Incomplete) => Parser(m.0, State::Incomplete(m.0, 1)),
        // Not parsed one yet
        // TODO: Better error
        (false, _)                   => Parser(m.0, State::Err(m.0, Error::Many1Fail)),
    }
}
