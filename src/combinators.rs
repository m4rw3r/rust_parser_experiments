use std::iter::FromIterator;

use Parser;
use State;
use parser;

use iter::{
    Iter,
    EndState,
};

/// Applies the parser ``p`` exactly ``num`` times, propagating any error or incomplete state.
#[inline]
pub fn count<'a, 'p, I, T, E, F, U, P>(num: usize, p: F) -> impl Parser<'a, I, T, E> + 'p
  where I: Copy,
        F: FnMut() -> P + 'p,
        P: Parser<'a, I, U, E>,
        T: FromIterator<U> {
    parser(move |i| {
        // If we have gotten an item, if this is false after from_iter we have failed
        let mut count = 0;
        let mut iter  = Iter::new(i, p);

        let result: T      = FromIterator::from_iter(iter.by_ref().take(num).inspect(|_| count = count + 1 ));
        let (buffer, last) = iter.end_state();

        if count == num {
            State::Item(buffer, result)
        } else {
            // Can only be less than num here since take() limits it.
            // Just propagate the last state from the iterator.
            match last {
                EndState::Incomplete(n) => State::Incomplete(n),
                EndState::Error(b, e)   => State::Error(b, e),
            }
        }
    })
}

/// Tries the parser ``f``, on success it yields the parsed value, on failure ``default`` will be
/// yielded instead.
/// 
/// Incomplete state is propagated.
#[inline]
pub fn option<'a, 'p, I, T, E, F>(f: F, default: T) -> impl Parser<'a, I, T, E> + 'p
  where I: 'a + Copy,
        T: 'p,
        F: Parser<'a, I, T, E> + 'p {
    parser(move |i| match f.parse(i) {
        State::Item(b, t)    => State::Item(b, t),
        State::Error(_, _)   => State::Item(i, default),
        State::Incomplete(n) => State::Incomplete(n),
    })
}

/// Tries to match the parser ``f``, if ``f`` fails it tries ``g``. Returns the success value of
/// the first match, otherwise the error of the last one if both fail.
/// 
/// Incomplete state is propagated from the first one to report incomplete.
#[inline]
pub fn or<'a, 'p, I, T, E, F, G>(f: F, g: G) -> impl Parser<'a, I, T, E> + 'p
  where I: Copy,
        F: Parser<'a, I, T, E> + 'p,
        G: Parser<'a, I, T, E> + 'p {
    parser(move |i| match f.parse(i) {
        State::Item(b, t)    => State::Item(b, t),
        State::Error(_, _)   => g.parse(i),
        State::Incomplete(r) => State::Incomplete(r),
    })
}

/// Parses many instances of ``f`` until it does no longer match, returning all matches.
/// 
/// Note: If the last parser succeeds on the last input item then this parser is still considered
/// incomplete as there might be more data to fill.
/// 
/// Note: Allocates data.
#[inline]
pub fn many<'a, 'p, I, T, E, F, U, P>(f: F) -> impl Parser<'a, I, T, E> + 'p
  where I: Copy,
        F: Fn() -> P + 'p,
        P: Parser<'a, I, U, E> + 'p,
        T: FromIterator<U> {
    parser(move |i| {
        let mut iter = Iter::new(i, f);

        let result: T = FromIterator::from_iter(iter.by_ref());

        match iter.end_state() {
            // Ok, last parser failed, we have iterated all.
            // Return remainder of buffer and the collected result
            (b, EndState::Error(_, _))   => State::Item(b, result),
            // Nested parser incomplete, propagate
            (_, EndState::Incomplete(n)) => State::Incomplete(n),
        }
    })
}

/// Parses at least one instance of ``f`` and continues until it does no longer match,
/// returning all matches.
/// 
/// Note: If the last parser succeeds on the last input item then this parser is still considered
/// incomplete as there might be more data to fill.
/// 
/// Note: Allocates data.
// TODO: Proper checking of return value in doc-test
#[inline]
pub fn many1<'a, 'p, I, T, E, F, U, P>(f: F) -> impl Parser<'a, I, T, E> + 'p where I: Copy, F: Fn() -> P + 'p, P: Parser<'a, I, U, E>, T: FromIterator<U> {
    parser(move |i| {
        // If we managed to parse anything
        let mut item = false;
        // If we have gotten an item, if this is false after from_iter we have failed
        let mut iter = Iter::new(i, f);

        let result: T = FromIterator::from_iter(iter.by_ref().inspect(|_| item = true ));

        if !item {
            match iter.end_state() {
                (_, EndState::Error(b, e))   => State::Error(b, e),
                (_, EndState::Incomplete(n)) => State::Incomplete(n),
            }
        } else {
            match iter.end_state() {
                (b, EndState::Error(_, _))   => State::Item(b, result),
                // TODO: Indicate potentially more than 1?
                (_, EndState::Incomplete(n)) => State::Incomplete(n),
            }
        }
    })
}

#[cfg(test)]
mod test {
    #[test]
    fn count() {
        use ::{Parser, count, char};

        let p = count(3, || char(b'a'));

        let r: Vec<u8> = p.parse(b"aaa").unwrap();

        assert_eq!(r, [b'a', b'a', b'a']);
    }

    #[test]
    fn option() {
        use ::{Parser, option, char};

        let p1 = option(char(b'c'), b'z');
        let p2 = option(char(b'a'), b'z');

        assert_eq!(p1.parse(b"abcd").unwrap(), b'z');
        assert_eq!(p2.parse(b"abcd").unwrap(), b'a');
    }

    #[test]
    fn or() {
        use ::{Parser, or, char};

        let p = or(char(b'b'), char(b'a'));

        assert_eq!(p.parse(b"abc").unwrap(), b'a');
    }

    #[test]
    fn many() {
        use ::{Parser, bind, char, many, ret, take_while1};

        let p = many(|| bind(take_while1(|c| c != b',' && c != b' '), move |c| bind(char(b','), move |_| ret(c))));

        let v: Vec<&[u8]> = p.parse(b"a,bc,cd ").unwrap();

        assert_eq!(v.len(), 2);
        assert_eq!(v[0], b"a");
        assert_eq!(v[1], b"bc");
    }

    #[test]
    fn many1() {
        use ::error;
        use ::{Error, Parser, State, bind, char, many1, ret, take_while1};

        let p = many1(|| bind(take_while1(|c| c != b',' && c != b' '), move |c| bind(char(b','), move |_| ret(c))));

        let r: State<_, Vec<_>, Error<_>> = p.parse(b"a ");

        assert_eq!(r.unwrap_err(), error::expected(b','));
    }
}
