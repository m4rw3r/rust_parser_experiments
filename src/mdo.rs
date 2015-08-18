// thanks to https://github.com/TeXitoi/rust-mdo for the inspiration

/// Macro for using monadic do like haskell's do-notation.
///
/// ```rust
/// #[macro_use]
/// extern crate parser;
/// 
/// fn main() {
///     use parser::{Parser, State, bind, ret, char, any};
///
///     let r = mdo!{
///         let first  = any();
///                      char(b':');
///         let second = any();
///
///         ret (first, second)
///     };
///
///     assert_eq!(r.parse(b"a:b"), (State::Item((b'a', b'b')), b"" as &[u8]));
///     assert_eq!(r.parse(b"a:b"), (State::Item((b'a', b'b')), b"" as &[u8]));
/// }
/// ```
#[macro_export]
macro_rules! mdo (
    // Return, typed
    ( ret $t:ty, $e:ty : $r:expr ) => {
        ret::<_, $t, $e>($r)
    };
    // Return
    ( ret $r:expr ) => {
        ret($r)
    };
    // Error, typed
    ( err <$r:ty , $e:ty> $e:expr ) => {
        err::<_, $r, $e>($e)
    };
    // Error
    ( err $e:expr ) => {
        err($e)
    };
    // Bind
    ( let $p:pat = $i:expr ; $( $t:tt )* ) => {
        bind($i, move |$p| mdo!{ $( $t )* })
    };
    // Bind, type
    ( let $p:ident : $ty:ty = $i:expr ; $( $t:tt )* ) => {
        bind($i, move |$p: $ty| mdo!{ $( $t )* })
    };
    // Then
    ( $i:expr; $( $t:tt )* ) => {
        bind($i, move |_| mdo!{ $( $t )* })
    };
);

/*#[cfg(test)]
mod test {
    use ::{
        Error,
        Parser,
        State,
        bind,
        satisfy,
        ret,
    };
    use ::parsers::{
        char,
        take_while1,
    };

    #[test]
    fn mdo() {
        fn is_digit(c: u8) -> bool {
            c >= b'0' && c <= b'9'
        }

        fn decimal<'a>(m: Parser<'a, u8, (), ()>) -> Parser<'a, u8, usize, Error<u8>> {
            mdo!{m,
                 bytes = take_while1(is_digit);

                 ret bytes.iter().fold(0, |a, b| a * 10 + (b - b'0') as usize)
            }
        }

        let m = From::from(b"123.4567 ");

        let Parser(buf, state) = mdo!(m,
            real = decimal;
                   char(b'.');
            frac = decimal;

            ret _, Error<_> : (real, frac)
        );

        assert_eq!(buf, &[b' ']);
        assert_eq!(state, State::Ok((123, 4567)));
    }
    
    #[test]
    fn mdo_closure() {
        fn decimal<'a>(m: Parser<'a, u8, (), ()>) -> Parser<'a, u8, usize, Error<u8>> {
            mdo!{m,
                 bytes = take_while1(|c| c >= b'0' && c <= b'9');

                 ret bytes.iter().fold(0, |a, b| a * 10 + (b - b'0') as usize)
            }
        }

        let m = From::from(b"123.4567 ");

        let Parser(buf, state) = mdo!(m,
            real = decimal;
                   satisfy(|c| c == b'.');
            frac = decimal;

            ret _, Error<_> : (real, frac)
        );

        assert_eq!(buf, &[b' ']);
        assert_eq!(state, State::Ok((123, 4567)));
    }
}*/
