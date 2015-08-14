// thanks to https://github.com/TeXitoi/rust-mdo for the inspiration

/// Macro for using monadic do like haskell's do-notation.
///
/// ```rust
/// #[macro_use]
/// extern crate parser;
/// 
/// fn main() {
///     use parser::{Parser, bind, ret, char, any, Error};
///
///     let m = From::from(b"a:b" as &[u8]);
///
///     let r: Parser<_, (u8, u8), parser::Error<_>> = mdo!{m,
///         first  = any;
///         char(b':');
///         second = any;
///         ret (first, second)
///     };
///
///     assert_eq!(r.unwrap(), (b'a', b'b'));
/// }
/// ```
#[macro_export]
macro_rules! mdo (
    // Return, typed
    ( $m:ident , ret $t:ty, $e:ty : $r:expr ) => {
        ret::<_, $t, $e>($m, $r)
    };
    // Return
    ( $m:ident , ret $r:expr ) => {
        ret($m, $r)
    };
    // Error, typed
    ( $m:ident , err <$r:ty , $e:ty> $e:expr ) => {
        err::<_, $r, $e>($m, $e)
    };
    // Error
    ( $m:ident , err $e:expr ) => {
        err($m, $e)
    };
    // Empty
    ( $m:ident , ) => {
        $m
    };
    // Bind
    ( $m:ident , $p:pat = $i:ident ; $( $t:tt )* ) => {
        bind($i($m), |$m, $p| mdo!{ $m, $( $t )* })
    };
    // Bind, type
    ( $m:ident , $p:ident : $ty:ty = $i:ident ; $( $t:tt )* ) => {
        bind($i($m), |$m, $p: $ty| mdo!{ $m, $( $t )* })
    };
    // Bind, params to $i
    ( $m:ident , $p:pat = $i:ident ($( $e:expr ),+); $( $t:tt )* ) => {
        bind($i($m, $( $e ),+), |$m, $p| mdo!{ $m, $( $t )* })
    };
    // Bind, params to $i, type
    ( $m:ident , $p:ident : $ty:ty = $i:ident ($( $e:expr ),+); $( $t:tt )* ) => {
        bind($i($m, $( $e ),+), |$m, $p: $ty| mdo!{ $m, $( $t )* })
    };
    // Then without parameters
    ( $m:ident , $i:ident; $( $t:tt )* ) => {
        bind($i($m), |$m, _| mdo!{ $m, $( $t )* })
    };
    // Then
    ( $m:ident , $i:ident ($( $e:expr ),+); $( $t:tt )* ) => {
        bind($i($m, $( $e ),+), |$m, _| mdo!{ $m, $( $t )* })
    };
);

#[cfg(test)]
mod test {
    use ::{
        Parser,
        State,
        bind,
        ret,
    };
    use ::parsers::{
        Error,
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

        let m = From::from(b"123.4567 " as &[u8]);

        let Parser(buf, state) = mdo!(m,
            real = decimal;
            char(b'.');
            frac = decimal;
            ret _, Error<_> : (real, frac)
        );

        assert_eq!(buf, &[b' ']);
        assert_eq!(state, State::Ok((123, 4567)));
    }
}
