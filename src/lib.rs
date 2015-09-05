#![feature(default_type_parameter_fallback)]

pub trait Parser<I, T, E> {
    fn parse<'a>(self, &'a [I]) -> State<'a, I, T, E>;
}

impl<I, T, E, F> Parser<I, T, E> for F
  where F: for<'p>FnOnce(&'p [I]) -> State<'p, I, T, E> {
    fn parse<'a>(self, i: &'a [I]) -> State<'a, I, T, E> {
        self(i)
    }
}

fn parser<'a, I, T, E, F>(p: F) -> impl Parser<I, T, E> + 'a
  where F: for<'p>FnOnce(&'p [I]) -> State<'p, I, T, E> + 'a {
    p
}

#[derive(Debug, Eq, PartialEq)]
#[must_use]
pub enum State<'a, I, T, E>
  where I: 'a {
    /// Success state, slice is the slice of the input data starting just after the last successful
    /// parser.
    Item(&'a [I], T),
    /// Error state, slice is the slice of the input data from the position where the error
    /// occurred.
    Error(&'a [I], E),
    // Do not include data-slice here as we do not need to actually return the part which is
    // incomplete. The parse has to restart anyway from the index where it was.
    Incomplete(usize),
}

pub mod monad {
    use ::Parser;
    use ::State;
    use ::parser;

    pub fn bind<'a, I, P, T, E, F, U, R, V = E>(p: P, f: F) -> impl Parser<I, U, V> + 'a
      where P: Parser<I, T, E> + 'a,
            F: FnOnce(T) -> R + 'a,
            R: Parser<I, U, V>,
            V: From<E> {
        parser(move |i| {
            match p.parse(i) {
                State::Item(b, t)    => f(t).parse(b),
                State::Error(b, e)   => State::Error(b, From::from(e)),
                State::Incomplete(n) => State::Incomplete(n),
            }
        })
    }

    pub fn ret<'a, I, T, E = ()>(a: T) -> impl Parser<I, T, E>+ 'a
      where T: 'a {
        parser(move |i| {
            State::Item(i, a)
        })
    }

    pub fn err<'a, I, T, E>(e: E) -> impl Parser<I, T, E> + 'a
      where E: 'a {
        parser(move |i| {
            State::Error(i, e)
        })
    }

    #[cfg(test)]
    mod test {
        use ::Parser;
        use ::State;
        use ::monad::{bind, ret, err};

        #[test]
        fn left_identity() {
            fn f<I>(i: u32) -> impl Parser<I, u32, ()>
              where I: Copy {
                ret(i + 1)
            }

            let a = 123;
            // return a >>= f
            let lhs = bind(ret(a), f);
            // f a
            let rhs = f(a);

            assert_eq!(lhs.parse(b"test"), State::Item(&b"test"[..], 124));
            assert_eq!(rhs.parse(b"test"), State::Item(&b"test"[..], 124));
        }

        #[test]
        fn right_identity() {
            let m1 = ret::<_, usize, ()>(1);
            let m2 = ret::<_, usize, ()>(1);

            let lhs = bind(m1, ret);
            let rhs = m2;

            assert_eq!(lhs.parse(b"test"), State::Item(&b"test"[..], 1));
            assert_eq!(rhs.parse(b"test"), State::Item(&b"test"[..], 1));
        }
        
        #[test]
        fn associativity() {
             fn f<I: Copy>(num: u32) -> impl Parser<I, u64, ()> {
                ret((num + 1) as u64)
            }

            fn g<I: Copy>(num: u64) -> impl Parser<I, u64, ()> {
                ret(num * 2)
            }

            let lhs_m = ret::<_, _, ()>(2);
            let rhs_m = ret::<_, _, ()>(2);

            // (m >>= f) >>= g
            let lhs = bind(bind(lhs_m, f), g);
            // m >>= (\x -> f x >> g)
            let rhs = bind(rhs_m, |x| bind(f(x), g));

            assert_eq!(lhs.parse(b"test"), State::Item(&b"test"[..], 6));
            assert_eq!(rhs.parse(b"test"), State::Item(&b"test"[..], 6));
        }
    }
}
