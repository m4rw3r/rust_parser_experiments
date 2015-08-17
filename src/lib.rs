#![feature(fnbox)]
use std::boxed::FnBox;

#[derive(Debug, Eq, PartialEq)]
pub enum State<T, E> {
    Item(T),
    Error(E),
    Incomplete(usize),
}

// type Parser<'a, I, T, E> = Fn(&'a [I]) -> (State<T, E>, &'a [I]);

pub struct Parser<'a, 'b, I, T, E>(Box<FnBox(&'a [I]) -> (State<T, E>, &'a [I]) + 'b>)
  where I: 'a + Clone, T: 'b;

impl<'a, 'b, I, T, E> Parser<'a, 'b, I, T, E>
  where I: 'a + Clone,
        T: 'b {
    pub fn parse(self, buf: &'a [I]) -> (State<T, E>, &'a [I]) {
        (self.0)(buf)
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
    Parser(Box::new(move |i| (State::Item(value), i)))
}

// TODO: Lifetimes are odd here, fix them, the contained value should not need to have exactly the
// same lifetime as the input stream
pub fn bind<'a, I, T, E, U, F>(p: Parser<'a, 'a, I, T, E>, f: F) -> Parser<'a, 'a, I, U, E>
  where I: 'a + Clone,
        T: 'a,
        U: 'a,
        E: 'a,
        F: Fn(T) -> Parser<'a, 'a, I, U, E> + 'a {
    Parser(Box::new(move |i| {
        let (a, is) = p.parse(i);

        match a {
            State::Item(t)       => f(t).parse(is),
            State::Error(e)      => (State::Error(e), is),
            State::Incomplete(n) => (State::Incomplete(n), is),
        }
    }))
}

#[cfg(test)]
mod test {
    use ::{Parser, State, bind, ret};

    #[test]
    fn left_identity() {
        fn f<'a, I>(i: u32) -> Parser<'a, 'a, I, u32, ()>
          where I: 'a + Clone {
            ret(i + 1)
        }

        let a = 123;
        // return a >>= f
        let lhs = bind(ret(a), f);
        // f a
        let rhs = f(a);

        assert_eq!(lhs.parse(b"test"), (State::Item(124), b"test" as &[u8]));
        assert_eq!(rhs.parse(b"test"), (State::Item(124), b"test" as &[u8]));
    }

    #[test]
    fn right_identity() {
        let m1: Parser<_, _, ()> = ret(1);
        let m2: Parser<_, _, ()> = ret(1);

        let lhs = bind(m1, ret);
        let rhs = m2;

        assert_eq!(lhs.parse(b"test"), (State::Item(1), b"test" as &[u8]));
        assert_eq!(rhs.parse(b"test"), (State::Item(1), b"test" as &[u8]));
    }
    
    #[test]
    fn associativity() {
         fn f<'a, I: 'a + Copy>(num: u32) -> Parser<'a, 'a, I, u64, ()> {
            ret((num + 1) as u64)
        }

        fn g<'a, I: 'a + Copy>(num: u64) -> Parser<'a, 'a, I, u64, ()> {
            ret(num * 2)
        }

        let lhs_m = ret(2);
        let rhs_m = ret(2);

        // (m >>= f) >>= g
        let lhs = bind(bind(lhs_m, f), g);
        // m >>= (\x -> f x >> g)
        let rhs = bind(rhs_m, |x| bind(f(x), g));

        assert_eq!(lhs.parse(b"test"), (State::Item(6), b"test" as &[u8]));
        assert_eq!(rhs.parse(b"test"), (State::Item(6), b"test" as &[u8]));
    }
}

pub mod parsers {
    use ::{Parser, State};

    #[derive(Debug, Eq, PartialEq)]
    pub enum Error<T: Copy> {
        Unexpected(T)
    }

    /// ```
    /// use parser::{Parser, State};
    /// use parser::parsers::satisfy;
    /// 
    /// assert_eq!(satisfy(|c| c == b'a').parse(b"abc"), (State::Item(b'a'), b"bc" as &[u8]));
    /// ```
    pub fn satisfy<'a, I: 'a + Copy, F>(f: F) -> Parser<'a, 'a, I, I, Error<I>>
      where F: Fn(I) -> bool + 'a {
        Parser(Box::new(move |i: &'a [I]| match i.first().map(|c| *c) {
            Some(c) if f(c) => (State::Item(c), &i[1..]),
            Some(c)         => (State::Error(Error::Unexpected(c)), i),
            None            => (State::Incomplete(1), i),
        }))
    }

    /// ```
    /// use parser::{Parser, State};
    /// use parser::parsers::any;
    /// 
    /// assert_eq!(any().parse(b"abc"), (State::Item(b'a'), b"bc" as &[u8]));
    /// ```
    pub fn any<'a, I: 'a + Copy>() -> Parser<'a, 'a, I, I, Error<I>> {
        satisfy(|_| true)
    }
}
