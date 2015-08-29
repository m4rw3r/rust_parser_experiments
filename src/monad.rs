use ::Parser;
use ::Input;
use ::State;

pub fn bind<'a, I, T, E, F, U, V = E>(m: Parser<'a, I, T, E>, f: F) -> Parser<'a, I, U, V>
  where I: 'a,
        V: From<E>,
        F: FnOnce(Input<'a, I>, T) -> Parser<'a, I, U, V> {
    match m.0 {
        State::Item(b, t)    => f(Input(b), t),
        State::Error(b, e)   => Parser(State::Error(b, From::from(e))),
        State::Incomplete(i) => Parser(State::Incomplete(i)),
    }
}

pub fn err<'a, I, T, E>(m: Input<'a, I>, e: E) -> Parser<'a, I, T, E> {
    Parser(State::Error(m.0, e))
}

pub fn ret<'a, I, T, E>(m: Input<'a, I>, t: T) -> Parser<'a, I, T, E> {
    Parser(State::Item(m.0, t))
}

#[cfg(test)]
mod test {
    use super::{
        bind,
        ret,
    };
    use ::Input;
    use ::Parser;
    use ::State;

    /// Test for the left identity monad law.
    /// 
    /// ```text
    /// return a >>= f ≡ f a
    /// ```
    #[test]
    fn left_identity() {
        // Using different types here just to make sure it typechecks correctly
        fn f<'a, I: 'a + Copy>(m: Input<'a, I>, num: u32) -> Parser<'a, I, u64, ()> {
            ret(m, (num + 1) as u64)
        }

        let a = 123;
        let l = From::from(b"abc");
        let r = From::from(b"abc");

        // return a >>= f
        let lhs = bind(ret(l, a), f);
        // f a
        let rhs = f(r, a);

        assert_eq!(lhs.0, State::Item(b"abc" as &[u8], 124u64));
        assert_eq!(rhs.0, State::Item(b"abc" as &[u8], 124u64));
    }

    /// Test for right identity monad law.
    /// 
    /// ```text
    /// m >>= return  ≡  m
    /// ```
    #[test]
    fn right_identity() {
        let m: Parser<_, _, ()> = Parser(State::Item(b"abc" as &[u8], "data"));

        // m >>= return
        // TODO: Can we do something to skip this type annotation?
        let lhs: Parser<_, _, ()> = bind(m, ret);
        // m
        let rhs = Parser(State::Item(b"abc" as &[u8], "data"));

        assert_eq!(lhs.0, rhs.0);
    }

    /// Test for associativity monad law.
    /// 
    /// ```text
    /// (m >>= f) >>= g  ≡  m >>= (\x -> f x >>= g)
    /// ```
    #[test]
    fn associativity() {
        fn f<'a, I: 'a + Copy>(m: Input<'a, I>, num: u32) -> Parser<'a, I, u64, ()> {
            ret(m, (num + 1) as u64)
        }

        fn g<'a, I: 'a + Copy>(m: Input<'a, I>, num: u64) -> Parser<'a, I, u64, ()> {
            ret(m, num * 2)
        }

        let l: Parser<_, _, ()> = Parser(State::Item(b"abc" as &[u8], 2));
        let r: Parser<_, _, ()> = Parser(State::Item(b"abc" as &[u8], 2));

        // (m >>= f) >>= g
        let lhs = bind(bind(l, f), g);
        // m >>= (\x -> f x >> g)
        let rhs = bind(r, |m, x| bind(f(m, x), g));

        assert_eq!(lhs.0, State::Item(b"abc" as &[u8], 6));
        assert_eq!(rhs.0, State::Item(b"abc" as &[u8], 6));
    }
}
