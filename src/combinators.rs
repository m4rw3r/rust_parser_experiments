//! Provided combinators

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

/// Matches the given parser exactly ``num`` times, returning all the matches.
/// 
/// Will be considered incomplete if any nested parser reports incomplete or if the input is not
/// enough to match exactly ``num`` times.
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
pub fn count<'a, I: 'a + Copy, T, E, F, U>(m: Empty<'a, I>, num: usize, f: F) -> Parser<'a, I, T, Error<I>>
  where F: Fn(Empty<'a, I>) -> Parser<'a, I, U, E>,
        T: FromIterator<U> {
    // If we have gotten an item, if this is false after from_iter we have failed
    let mut count = 0;
    let mut iter  = Iter::new(m.0, f);

    let result: T = FromIterator::from_iter(iter.by_ref().take(num).inspect(|_| count = count + 1 ));

    match (count, iter.last_state()) {
        (i, IResult::Good) if i == num => Parser(iter.buffer(), State::Ok(result)),
        (_, IResult::Good)             => if iter.buffer().len() > 0 {
                Parser(m.0, State::Err(m.0, Error::ExpectedCount(num)))
            } else {
                Parser(m.0, State::Incomplete(m.0, 1))
            },
        (_, IResult::Incomplete) => Parser(m.0, State::Incomplete(m.0, 1)),
        // TODO: Propagate inner error
        (_, IResult::Bad)        => Parser(m.0, State::Err(m.0, Error::ExpectedCount(num))),
    }
}

/// Tries the parser ``f``, on success it yields the parsed value, on failure ``default`` will be
/// yielded instead.
/// 
/// Incomplete state is propagated.
/// 
/// ```
/// use parser::{Parser, option, char};
/// 
/// let p1 = From::from(b"abcd");
/// let p2 = From::from(b"abcd");
/// 
/// assert_eq!(option(char(p1, b'c'), b'z').unwrap(), b'z');
/// assert_eq!(option(char(p2, b'a'), b'z').unwrap(), b'a');
/// ```
#[inline]
pub fn option<'a, I: 'a + Copy, T, E>(m: Parser<'a, I, T, E>, default: T) -> Parser<'a, I, T, E> {
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
#[inline]
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
#[inline]
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

/// Runs the given parser until it fails, discarding matched input.
/// 
/// Incomplete state will be propagated.
/// 
/// This is more efficient to use compared to using ``many`` and then just discarding the result as
/// ``many`` allocates a separate data structure to contain the data before proceeding.
/// 
/// ```
/// use parser::{Parser, bind, skip_many, char};
/// 
/// let p = From::from(b"aaaabc");
/// 
/// assert_eq!(bind(skip_many(p, |m| char(m, b'a')), |m, _| char(m, b'b')).unwrap(), b'b');
/// ```
#[inline]
pub fn skip_many<'a, I: 'a + Copy, T, E, F>(m: Empty<'a, I>, f: F) -> Parser<'a, I, (), Error<I>>
  where F: Fn(Empty<'a, I>) -> Parser<'a, I, T, E> {
    let Parser(buf_orig, _) = m;

    let mut buf = buf_orig;

    loop {
        match f(Parser(buf, State::Ok(()))) {
            Parser(b, State::Ok(_))            => buf = b,
            Parser(_, State::Err(_, _))        => break,
            Parser(_, State::Incomplete(i, n)) => return Parser(buf_orig, State::Incomplete(i, n)),
        }
    }

    Parser(buf, State::Ok(()))
}
