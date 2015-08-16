use std::marker::PhantomData;

#[derive(Debug, Eq, PartialEq)]
pub enum State<T, E> {
    Item(T),
    Error(E),
    Incomplete(usize),
}

pub trait Parser<'a, I: Copy, T, E> {
    fn parse(&self, &'a [I]) -> (State<T, E>, &'a [I]);
}

/// ```
/// use parser::{Parser, Char, State, bind};
/// 
/// assert_eq!(bind(Char, |_| Char).parse(b"abc"), (State::Item(b'b'), b"c" as &[u8]));
/// ```
pub struct Bind<'a, I: 'a + Copy, T, E, U, V, P, F>
  where V: From<E>,
        P: Parser<'a, I, T, E>,
        F: Fn(T) -> Parser<'a, I, U, V> {
    p: P,
    f: Box<F>,
    _a: PhantomData<&'a [I]>,
    _t: PhantomData<T>,
    _e: PhantomData<E>,
    _u: PhantomData<U>,
    _v: PhantomData<V>,
}

impl<'a, I: 'a + Copy, T, E, U, V, P, F> Parser<'a, I, U, V> for Bind<'a, I, T, E, U, V, P, F>
  where V: From<E>,
        P: Parser<'a, I, T, E>,
        F: Fn(T) -> Parser<'a, I, U, V> {
    fn parse(&self, buf: &'a [I]) -> (State<U, V>, &'a [I]) {
        match self.p.parse(buf) {
            (State::Item(t), a)       => (self.f)(t).parse(a),
            (State::Error(e), a)      => (State::Error(From::from(e)), a),
            (State::Incomplete(i), a) => (State::Incomplete(i), a),
        }
    }
}

pub fn bind<'a, I: 'a + Copy, T, E, U, V, P, F>(p: P, f: F) -> Bind<'a, I, T, E, U, V, P, F>
  where V: From<E>,
        P: Parser<'a, I, T, E>,
        F: Fn(T) -> Parser<'a, I, U, V> {
    Bind{ p: p, f: Box::new(f), _a: PhantomData, _t: PhantomData, _e: PhantomData, _u: PhantomData, _v: PhantomData }
}

pub struct Ret<T: Copy>(pub T);

impl<'a, I: Copy, T: Copy> Parser<'a, I, T, ()> for Ret<T> {
    fn parse(&self, buf: &'a [I]) -> (State<T, ()>, &'a [I]) {
        (State::Item(self.0), buf)
    }
}

pub struct Char;

impl<'a, I: Copy> Parser<'a, I, I, ()> for Char {
    fn parse(&self, buf: &'a [I]) -> (State<I, ()>, &'a [I]) {
        match buf.first() {
            Some(&c) => (State::Item(c),       &buf[1..]),
            None     => (State::Incomplete(1), buf),
        }
    }
}

/// ```
/// use parser::{Parser, take_while, State};
/// 
/// assert_eq!(take_while(|c| c == b'a').parse(b"aaabb"), (State::Item(b"aaa" as &[u8]), b"bb" as &[u8]));
/// ```
pub struct TakeWhile<I: Copy, F: Fn(I) -> bool>(F, PhantomData<I>);

pub fn take_while<I: Copy, F: Fn(I) -> bool>(f: F) -> TakeWhile<I, F> {
    TakeWhile(f, PhantomData)
}

impl<'a, I: Copy, F> Parser<'a, I, &'a [I], ()> for TakeWhile<I, F>
  where F: Fn(I) -> bool {
    fn parse(&self, buf: &'a [I]) -> (State<&'a [I], ()>, &'a [I]) {
        match buf.iter().position(|&c| self.0(c) != true) {
            Some(n) => (State::Item(&buf[..n]), &buf[n..]),
            None    => (State::Incomplete(1), buf),
        }
    }
}

/// 
/// ```
/// use parser::{Parser, take_while1, State};
/// 
/// assert_eq!(take_while1(|c| c == b'a').parse(b"aaabb"), (State::Item(b"aaa" as &[u8]), b"bb" as &[u8]));
/// ```
pub struct TakeWhile1<I: Copy, F: Fn(I) -> bool>(F, PhantomData<I>);

pub fn take_while1<I: Copy, F: Fn(I) -> bool>(f: F) -> TakeWhile1<I, F> {
    TakeWhile1(f, PhantomData)
}

impl<'a, I: Copy, F> Parser<'a, I, &'a [I], ()> for TakeWhile1<I, F>
  where F: Fn(I) -> bool {
    fn parse(&self, buf: &'a [I]) -> (State<&'a [I], ()>, &'a [I]) {
        match buf.iter().position(|&c| self.0(c) != true) {
            Some(0) => (State::Error(()), buf),
            Some(n) => (State::Item(&buf[..n]), &buf[n..]),
            None    => (State::Incomplete(1), buf),
        }
    }
}
