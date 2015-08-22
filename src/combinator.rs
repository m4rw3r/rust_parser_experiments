use std::iter::FromIterator;

use Input;
use Parser;
use State;
use Error;

use iter::Iter;
use iter::IResult;

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
        // Ok, last parser failed, we have iterated all
        IResult::Bad        => Parser(State::Item(iter.buffer(), result)),
        // Nested parser incomplete, propagate
        IResult::Incomplete => Parser(State::Incomplete(1)),
    }
}

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
        (true, IResult::Good)       => Parser(State::Incomplete(1)),
        // Ok, last parser failed, we have iterated all
        (true, IResult::Bad)        => Parser(State::Item(iter.buffer(), result)),
        // Nested parser incomplete, propagate
        (_,    IResult::Incomplete) => Parser(State::Incomplete(1)),
        (false, _)                  => Parser(State::Error(m.0, Error::Many1)),
    }
}
