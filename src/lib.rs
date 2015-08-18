pub mod mdo;

pub use combinators::{
    or,
    many,
    many1,
};

pub use parsers::{
    satisfy,
    any,
    char,
    take_while,
    take_while1,
    take_till,
    string,
};

#[derive(Debug, Eq, PartialEq)]
pub enum State<T, E> {
    Item(T),
    Error(E),
    Incomplete(usize),
}

// type Parser<'a, I, T, E> = Fn(&'a [I]) -> (State<T, E>, &'a [I]);

pub struct Parser<'a, 'b, I, T, E>(Box<Fn(&'a [I]) -> (State<T, E>, &'a [I]) + 'b>)
  where I: 'a + Copy,
        T: 'b;

impl<'a, 'b, I, T, E> Parser<'a, 'b, I, T, E>
  where I: 'a + Copy,
        T: 'b {
    #[inline]
    pub fn parse(&self, buf: &'a [I]) -> (State<T, E>, &'a [I]) {
        self.0(buf)
    }
}

/// ```
/// use parser::{Parser, State, ret};
/// 
/// let v: Parser<_, _, ()> = ret("a");
/// 
/// assert_eq!(v.parse(b"test"), (State::Item("a"), b"test" as &[u8]));
/// ```
#[inline]
pub fn ret<'a, 'b, I, T, E>(value: T) -> Parser<'a, 'b, I, T, E>
  where I: 'a + Copy,
        T: 'b + Clone {
    Parser(Box::new(move |i| (State::Item(value.clone()), i)))
}

// TODO: Lifetimes are odd here, fix them, the contained value should not need to have exactly the
// same lifetime as the input stream
#[inline]
pub fn bind<'a, I, T, E, U, F>(p: Parser<'a, 'a, I, T, E>, f: F) -> Parser<'a, 'a, I, U, E>
  where I: 'a + Copy,
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
          where I: 'a + Copy {
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

pub mod combinators {
    use std::iter::FromIterator;

    use ::{Error, Parser, State};
    use ::iter::{IResult, Iter};

    /// mplus.
    /// 
    /// ```
    /// use parser::{Parser, State, char, or};
    /// 
    /// assert_eq!(or(char(b'a'), char(b'b')).parse(b"abc"), (State::Item(b'a'), b"bc" as &[u8]));
    /// assert_eq!(or(char(b'b'), char(b'a')).parse(b"abc"), (State::Item(b'a'), b"bc" as &[u8]));
    /// ```
    #[inline]
    pub fn or<'a, I: 'a + Copy, T, E: 'a>(a: Parser<'a, 'a, I, T, E>, b: Parser<'a, 'a, I, T, E>) -> Parser<'a, 'a, I, T, E> {
        Parser(Box::new(move |i| {
            match a.parse(i) {
                (State::Item(t), is)       => (State::Item(t), is),
                (State::Error(_), _)       => b.parse(i),
                (State::Incomplete(n), is) => (State::Incomplete(n), is),
            }
        }))
    }

    /// ```
    /// use parser::{Error, State, char, many};
    /// 
    /// let a: (State<Vec<u8>, _>, _) = many(char(b'a')).parse(b"aab");
    /// let b: (State<Vec<u8>, _>, _) = many(char(b'a')).parse(b"bcd");
    /// 
    /// assert_eq!(a, (State::Item(vec![b'a', b'a']), b"b" as &[u8]));
    /// assert_eq!(b, (State::Item(vec![]), b"bcd" as &[u8]));
    /// ```
    #[inline]
    pub fn many<'a, I: 'a + Copy, T: 'a, E: 'a, U>(parser: Parser<'a, 'a, I, U, E>) -> Parser<'a, 'a, I, T, Error<I>>
      where T: FromIterator<U> {
        Parser(Box::new(move |i: &'a [I]| {
            let mut iter = Iter::new(i, &parser);

            let result: T = FromIterator::from_iter(iter.by_ref());

            match iter.last_state() {
                // We haven't read everything yet
                IResult::Good       => (State::Incomplete(1), i),
                // Ok, last parser failed, we have iterated all
                IResult::Bad        => (State::Item(result), iter.buffer()),
                // Nested parser incomplete, propagate
                IResult::Incomplete => (State::Incomplete(1), i),
            }
        }))
    }

    /// ```
    /// use parser::{Error, State, char, many1};
    /// 
    /// let a: (State<Vec<u8>, _>, _) = many1(char(b'a')).parse(b"aab");
    /// let b: (State<Vec<u8>, _>, _) = many1(char(b'a')).parse(b"bcd");
    /// 
    /// assert_eq!(a, (State::Item(vec![b'a', b'a']), b"b" as &[u8]));
    /// assert_eq!(b, (State::Error(Error::Many1Fail), b"bcd" as &[u8]));
    /// ```
    #[inline]
    pub fn many1<'a, I: 'a + Copy, T: 'a, E: 'a, U>(parser: Parser<'a, 'a, I, U, E>) -> Parser<'a, 'a, I, T, Error<I>>
      where T: FromIterator<U> {
        Parser(Box::new(move |i: &'a [I]| {
            // If we have gotten an item, if this is false after from_iter we have failed
            let mut item = false;
            let mut iter = Iter::new(i, &parser);

            let result: T = FromIterator::from_iter(iter.by_ref().inspect(|_| item = true ));

            match (item, iter.last_state()) {
                // We haven't read everything yet
                (true,  IResult::Good)       => (State::Incomplete(1), i),
                // Ok, last parser failed, we have iterated all
                (true,  IResult::Bad)        => (State::Item(result), iter.buffer()),
                // Nested parser incomplete, propagate
                (_,     IResult::Incomplete) => (State::Incomplete(1), i),
                // Not parsed one yet
                // TODO: Better error
                (false, _)                   => (State::Error(Error::Many1Fail), i),
            }
        }))
    }
}

pub mod iter {
    use ::{Parser, State};

    #[derive(Copy, Clone, Debug, Eq, PartialEq)]
    // TODO: Remove this from public
    pub enum IResult {
        Good,
        Bad,
        Incomplete,
    }

    pub struct Iter<'a: 'b, 'b, I: 'a + Copy, T: 'b, E: 'b> {
        last:   IResult,
        parser: &'b Parser<'a, 'b, I, T, E>,
        buf:    &'a [I]
    }

    impl<'a: 'b, 'b, I: 'a + Copy, T: 'b, E: 'b> Iter<'a, 'b, I, T, E> {
        #[inline]
        pub fn new(buffer: &'a [I], parser: &'b Parser<'a, 'b, I, T, E>) -> Self {
            Iter {
                last:   IResult::Good,
                parser: parser,
                buf:    buffer,
            }
        }

        #[inline]
        pub fn last_state(&self) -> IResult {
            self.last
        }

        #[inline]
        pub fn buffer(&self) -> &'a [I] {
            self.buf
        }
    }

    impl<'a, 'b, I: 'a + Copy, T: 'b, E> Iterator for Iter<'a, 'b, I, T, E> {
        type Item = T;

        #[inline]
        fn next(&mut self) -> Option<T> {
            if self.last != IResult::Good {
                return None
            }

            let (r, b) = self.parser.parse(self.buf);

            match r {
                State::Item(v)      => {
                    self.last = IResult::Good;
                    self.buf  = b;

                    Some(v)
                },
                State::Error(_)      => { self.last = IResult::Bad;        None },
                State::Incomplete(_) => { self.last = IResult::Incomplete; None },
            }
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum Error<T: Copy> {
    Unexpected(T),
    Expected(T, T),
    Many1Fail,
}

pub mod parsers {
    use ::{Error, Parser, State};

    /// ```
    /// use parser::{Parser, State, satisfy};
    /// 
    /// assert_eq!(satisfy(|c| c == b'a').parse(b"abc"), (State::Item(b'a'), b"bc" as &[u8]));
    /// ```
    #[inline]
    pub fn satisfy<'a, 'b, I: 'a + 'b + Copy, F>(f: F) -> Parser<'a, 'b, I, I, Error<I>>
      where F: Fn(I) -> bool + 'b {
        Parser(Box::new(move |i: &'a [I]| match i.first().map(|c| *c) {
            Some(c) if f(c) => (State::Item(c), &i[1..]),
            Some(c)         => (State::Error(Error::Unexpected(c)), i),
            None            => (State::Incomplete(1), i),
        }))
    }

    /// ```
    /// use parser::{Parser, State, any};
    /// 
    /// assert_eq!(any().parse(b"abc"), (State::Item(b'a'), b"bc" as &[u8]));
    /// ```
    #[inline]
    pub fn any<'a, I: 'a + Copy>() -> Parser<'a, 'a, I, I, Error<I>> {
        satisfy(|_| true)
    }

    /// ```
    /// use parser::{Parser, State, char};
    /// 
    /// assert_eq!(char(b'a').parse(b"abc"), (State::Item(b'a'), b"bc" as &[u8]));
    /// ```
    #[inline]
    pub fn char<'a, I: 'a + Copy + Eq>(chr: I) -> Parser<'a, 'a, I, I, Error<I>> {
        satisfy(move |c| c == chr)
    }

    /// ```
    /// use parser::{Error, Parser, State, take_while};
    /// 
    /// assert_eq!(take_while(|c| c == b'a').parse(b"aabc"),
    ///            (State::Item(b"aa" as &[u8]), b"bc" as &[u8]));
    /// assert_eq!(take_while(|c| c == b'a').parse(b"bcd"),
    ///            (State::Item(b"" as &[u8]), b"bcd" as &[u8]));
    /// ```
    #[inline]
    pub fn take_while<'a, 'b, I: 'a + Copy, P>(p: P) -> Parser<'a, 'b, I, &'a [I], Error<I>>
      where P: Fn(I) -> bool + 'b {
        Parser(Box::new(move |i: &'a [I]| match i.iter().position(|c| p(*c) == false) {
            Some(n) => (State::Item(&i[..n]), &i[n..]),
            None    => (State::Incomplete(1), i),
        }))
    }

    /// ```
    /// use parser::{Error, Parser, State, take_while1};
    /// 
    /// assert_eq!(take_while1(|c| c == b'a').parse(b"aabc"),
    ///            (State::Item(b"aa" as &[u8]), b"bc" as &[u8]));
    /// assert_eq!(take_while1(|c| c == b'a').parse(b"bcd"),
    ///            (State::Error(Error::Unexpected(b'b')), b"bcd" as &[u8]));
    /// ```
    #[inline]
    pub fn take_while1<'a, 'b, I: 'a + Copy, P>(p: P) -> Parser<'a, 'b, I, &'a [I], Error<I>>
      where P: Fn(I) -> bool + 'b {
        Parser(Box::new(move |i: &'a [I]| match i.iter().position(|c| p(*c) == false) {
            Some(0) => (State::Error(Error::Unexpected(i[0])), i),
            Some(n) => (State::Item(&i[..n]), &i[n..]),
            None    => (State::Incomplete(1), i),
        }))
    }

    /// ```
    /// use parser::{Error, Parser, State, take_till};
    /// 
    /// assert_eq!(take_till(|c| c == b'b').parse(b"aabc"),
    ///            (State::Item(b"aa" as &[u8]), b"bc" as &[u8]));
    /// assert_eq!(take_till(|c| c == b'b').parse(b"bcd"),
    ///            (State::Item(b"" as &[u8]), b"bcd" as &[u8]));
    /// ```
    #[inline]
    pub fn take_till<'a, 'b, I: 'a + Copy, P>(p: P) -> Parser<'a, 'b, I, &'a [I], Error<I>>
      where P: Fn(I) -> bool + 'b {
        Parser(Box::new(move |i: &'a [I]| match i.iter().position(|c| p(*c)) {
            Some(n) => (State::Item(&i[..n]), &i[n..]),
            None    => (State::Incomplete(1), i),
        }))
    }

    /// ```
    /// use parser::{Error, Parser, State, string};
    /// 
    /// assert_eq!(string(b"test").parse(b"testab"),
    ///            (State::Item(b"test" as &[u8]), b"ab" as &[u8]));
    /// assert_eq!(string(b"test").parse(b"tesabc"),
    ///            (State::Error(Error::Expected(b't', b'a')), b"tesabc" as &[u8]));
    /// ```
    #[inline]
    pub fn string<'a, 'b, I: 'a + Copy + Eq>(s: &'b [I]) -> Parser<'a, 'b, I, &'a [I], Error<I>> {
        Parser(Box::new(move |i: &'a [I]| {
            if i.len() < s.len() {
                return (State::Incomplete(s.len() - i.len()), i);
            }

            let d = &i[..s.len()];

            for j in 0..s.len() {
                if s[j] != d[j] {
                    return (State::Error(Error::Expected(s[j], d[j])), i);
                }
            }

            (State::Item(d), &i[s.len()..])
        }))
    }
}
