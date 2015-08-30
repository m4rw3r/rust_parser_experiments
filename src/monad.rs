//! Monad implementation for ``Parser``.
//! 
//! This monad implementation requires manual threading of the monad state to the nested
//! function ``f`` given to ``bind``. It is necessary since it is an eager monad and the nested
//! function ``f`` cannot return an action to be completed later in any effective way while
//! still allowing for ``rustc`` to optimize the code well. Earlier experiments were made with
//! laziness built-in, but they did not perform as well or had issues with ergonomics.
//! 
//! # Equivalence with Haskell's ``Monad`` typeclass:
//! 
//! ```text
//! f >>= g   ===  bind(f(m), g)
//! f >> g    ===  bind(f(m), |m, _| g(m))
//! return a  ===  ret(m, a)
//! fail a    ===  err(m, a)
//! ```
//!
//! Do-notation is provided by the macro ``mdo!``.
//!
//! # Satisfies the monad laws:
//! 
//! Left identity:  ``return a >>= f   ≡  f a``
//! 
//! Right identity: ``m >>= return     ≡  m``
//! 
//! Associativity:  ``(m >>= f) >>= g  ≡  m >>= (\x -> f x >>= g)``use ::Parser;

use ::Parser;
use ::Input;
use ::State;
use ::Error;

/// Takes the value of ``m`` and passes it to ``f`` which is run in a parser-context.
/// 
/// The first parameter to ``f`` is the current state of the parser, this is used to continue
/// parsing and to preserve state. It will ultimately be returned all the way back to the caller
/// of the parser provided that no errors were encountered.
/// 
/// The second parameter to ``f`` is the value in ``m``. This can be acted upon in any way.
/// 
/// If ``m`` is not in a success-state, then ``f`` will not be called and the error will be
/// passed through and potentially converted using ``From::from`` if need be.
/// 
/// ```rust
/// use parser::Parser;
/// use parser::monad::{ret, bind};
/// 
/// let p: Parser<_, _, ()> = ret(From::from(b"abc"), "Some value".to_string());
/// 
/// let r: Parser<_, _, ()> = bind(p, |m, s| ret(m, s + ": wohoo!"));
/// 
/// assert_eq!(r.unwrap(), "Some value: wohoo!");
/// ```
#[inline]
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

/// Runs the function ``f`` in the parser-context ``m``, replacing any existing value with the
/// result of ``f``.
/// 
/// The only parameter to ``f`` is the current state of the parser, this is used to continue
/// parsing and to preserve state. It will ultimately be returned all the way back to the caller
/// of the parser provided that no errors were encountered.
/// 
/// ```
/// use parser::Parser;
/// use parser::monad::{ret, then};
/// 
/// let p: Parser<_, _, ()> = ret(From::from(b"abc"), "Some value");
/// 
/// let r: Parser<_, _, ()> = then(p, |m| ret(m, "Wohoo!"));
/// 
/// assert_eq!(r.unwrap(), "Wohoo!");
/// ```
#[inline]
pub fn then<'a, I, T, E, F, U, V = E>(m: Parser<'a, I, T, E>, f: F) -> Parser<'a, I, U, V>
  where I: 'a,
        V: From<E>,
        F: FnOnce(Input<'a, I>) -> Parser<'a, I, U, V> {
    bind(m, |i, _| f(i))
}

/// Injects an error state into the parser-context ``m``.
/// 
/// ```
/// use parser::Parser;
/// use parser::monad::err;
/// 
/// let p: Parser<_, (), _> = err(From::from(b"abc"), "The Error!");
/// 
/// assert_eq!(p.unwrap_err(), "The Error!");
/// ```
#[inline]
pub fn err<'a, I, T, E>(m: Input<'a, I>, e: E) -> Parser<'a, I, T, E> {
    Parser(State::Error(m.0, e))
}

/// Injects a value in the parser-context ``m``.
/// 
/// ```
/// use parser::Parser;
/// use parser::monad::ret;
/// 
/// let p: Parser<_, _, ()> = ret(From::from(b"abc"), "value");
/// 
/// assert_eq!(p.unwrap(), "value");
/// ```
#[inline]
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
