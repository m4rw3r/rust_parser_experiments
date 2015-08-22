use Parser;
use State;
use Input;
use error::Error;

#[inline]
pub fn any<'a, I>(m: Input<'a, I>) -> Parser<'a, I, I, Error<I>>
  where I: Copy {
    satisfy(m, |_| true)
}

#[inline]
pub fn char<'a, I>(m: Input<'a, I>, chr: I) -> Parser<'a, I, I, Error<I>>
  where I: Copy + Eq {
    // TODO: Error with expected char
    satisfy(m, |c| c == chr)
}

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

#[inline]
pub fn string<'a, I>(m: Input<'a, I>, s: &[I]) -> Parser<'a, I, &'a [I], Error<I>>
  where I: Copy + Eq {
    if s.len() > m.0.len() {
        return Parser(State::Incomplete(s.len() - m.0.len()));
    }

    let d = &m.0[..s.len()];

    for i in 0..s.len() {
        if s[i] != d[i] {
            return Parser(State::Error(&m.0[i..], Error::Expected(s[i])));
        }
    }

    Parser(State::Item(&m.0[s.len()..], d))
}
