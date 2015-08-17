#![feature(fnbox)]
use std::boxed::FnBox;
use std::marker::PhantomData;

#[derive(Debug, Eq, PartialEq)]
pub enum State<T, E> {
    Item(T),
    Error(E),
    Incomplete(usize),
}

// type Parser<'a, I, T, E> = Fn(&'a [I]) -> (State<T, E>, &'a [I]);

pub struct Parser<'a, 'b, I, T, E>
  where I: 'a + Clone,
        T: 'b {
    f:  Box<FnBox(&'a [I]) -> (State<T, E>, &'a [I]) + 'b>,
}

impl<'a, 'b, I, T, E> Parser<'a, 'b, I, T, E>
  where I: 'a + Clone,
        T: 'b {
    pub fn parse(self, buf: &'a [I]) -> (State<T, E>, &'a [I]) {
        (self.f)(buf)
    }
}

/// ```
/// use parser::{Parser, State, ret};
/// 
/// let v: Parser<_, _, ()> = ret("a");
/// 
/// assert_eq!(v.parse(b"test"), (State::Item("a"), b"test" as &[u8]));
/// ```
pub fn ret<'a, 'b, I, T, E>(value: T) -> Parser<'a, 'b, I, T, E>
  where I: 'a + Clone,
        T: 'b {
    Parser{ f: Box::new(move |i| (State::Item(value), i)) }
}

/// ```
/// use parser::{Parser, State, bind, ret};
/// 
/// fn f<'a, I>(i: u32) -> Parser<'a, 'a, I, u32, ()>
///   where I: 'a + Clone {
///     ret(i + 1)
/// }
/// 
/// let a = 123;
/// let lhs = bind(ret(a), f);
/// let rhs = f(a);
/// 
/// assert_eq!(lhs.parse(b"test"), (State::Item(124), b"test" as &[u8]));
/// assert_eq!(rhs.parse(b"test"), (State::Item(124), b"test" as &[u8]));
/// ```
/// 
/// ```
/// use parser::{Parser, State, bind, ret};
/// 
/// let m1: Parser<_, _, ()> = ret(1);
/// let m2: Parser<_, _, ()> = ret(1);
/// 
/// let lhs = bind(m1, ret);
/// let rhs = m2;
/// 
/// assert_eq!(lhs.parse(b"test"), (State::Item(1), b"test" as &[u8]));
/// assert_eq!(rhs.parse(b"test"), (State::Item(1), b"test" as &[u8]));
/// ```
pub fn bind<'a, I, T, E, U, F>(p: Parser<'a, 'a, I, T, E>, f: F) -> Parser<'a, 'a, I, U, E>
  where I: 'a + Clone,
        T: 'a,
        U: 'a,
        E: 'a,
        F: Fn(T) -> Parser<'a, 'a, I, U, E> + 'a {
    Parser{ f: Box::new(move |i| {
        let (a, is) = p.parse(i);

        match a {
            State::Item(t)       => f(t).parse(is),
            State::Error(e)      => (State::Error(e), is),
            State::Incomplete(n) => (State::Incomplete(n), is),
        }
    })}
}
