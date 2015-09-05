use ::Parser;
use ::State;
use ::Error;
use ::parser;

#[inline]
pub fn bind<'a, 'p, I, P, T, E, F, U, R, V = E>(p: P, f: F) -> impl Parser<'a, I, U, V> + 'p
  where P: Parser<'a, I, T, E> + 'p,
        F: FnOnce(T) -> R + 'p,
        R: Parser<'a, I, U, V>,
        V: From<E> {
    parser(move |i| {
        match p.parse(i) {
            State::Item(b, t)    => f(t).parse(b),
            State::Error(b, e)   => State::Error(b, From::from(e)),
            State::Incomplete(n) => State::Incomplete(n),
        }
    })
}

#[inline]
pub fn ret<'a, 'p, I, T, E = Error<I>>(a: T) -> impl Parser<'a, I, T, E> + 'p
  where T: 'p {
    parser(move |i| {
        State::Item(i, a)
    })
}

#[inline]
pub fn err<'a, 'p, I, T, E>(e: E) -> impl Parser<'a, I, T, E> + 'p
  where E: 'p {
    parser(move |i| {
        State::Error(i, e)
    })
}

#[cfg(test)]
mod test {
    use ::Parser;
    use ::State;
    use ::monad::{bind, ret};

    #[test]
    fn left_identity() {
        fn f<'a, I>(i: u32) -> impl Parser<'a, I, u32, ()>
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

        let lhs = bind(m1, ret::<_, _, ()>);
        let rhs = m2;

        assert_eq!(lhs.parse(b"test"), State::Item(&b"test"[..], 1));
        assert_eq!(rhs.parse(b"test"), State::Item(&b"test"[..], 1));
    }
    
    #[test]
    fn associativity() {
         fn f<'a, I: Copy>(num: u32) -> impl Parser<'a, I, u64, ()> {
            ret((num + 1) as u64)
        }

        fn g<'a, I: Copy>(num: u64) -> impl Parser<'a, I, u64, ()> {
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
