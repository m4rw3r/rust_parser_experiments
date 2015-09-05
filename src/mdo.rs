// thanks to https://github.com/TeXitoi/rust-mdo for the inspiration

#[macro_export]
macro_rules! mdo {
    // Nonterminals:

    // mdo!(m, let a = f(); ...)       => bind(f(m), |m, a| ...)
    // mdo!(m, let a = f(b); ...)      => bind(f(m, b), |m, a| ...)
    // mdo!(m, let (a, b) = f(); ...)  => bind(f(m), |m, (a, b)| ...)
    ( let $a:pat = $f:expr ; $($r:tt)* ) => {
        bind( $f, move |$a| {
             mdo!{ $($r)* }
        })
    };

    // mdo!(m, let a:Foo = f(); ...)   => bind(f(m), |m, a: Foo| ...)
    // mdo!(m, let a:Foo = f(b); ...)  => bind(f(m, b), |m, a: Foo| ...)
    ( let $a:ident : $t:ty = $f:expr ; $($r:tt)* ) => {
        bind( $f, move |$a : $t| {
             mdo!{ $($r)* }
        })
    };

    // mdo!(m, f(b); ...}  =>  mdo!(f(m, b), ...)
    ( $f:expr ; $($r:tt)* ) => {
        bind( $f, move |_|
            mdo!{ $($r)* }
        )
    };

    // Terminals:

    // mdo!(m, ret u32, () : a)  =>  ret::<_, u32, ()>(m, a)
    ( ret $t:ty, $e:ty : $r:expr ) => {
        ret::<_, $t , $e >( $r )
    };

    // mdo!(m, ret a)  =>  ret(m, a)
    ( ret $r:expr ) => {
        ret( $r )
    };

    // mdo!(m, err a)  =>  err(m, a)
    ( err $e:expr ) => {
        err( $e )
    };

    // mdo!(m, err u32, () : a)  =>  err::<_, u32, ()>(m, a)
    ( err $r:ty , $e:ty :  $e:expr ) => {
        err::<_, $r , $e >( $e )
    };
}

#[cfg(test)]
mod test {
    use Parser;
    use State;
    use error::Error;
    use monad::{bind, ret};
    use parsers::{char, take_while1, satisfy};

    #[test]
    fn mdo() {
        fn is_digit(c: u8) -> bool {
            c >= b'0' && c <= b'9'
        }

        fn decimal<'a>() -> impl Parser<'a, u8, usize, Error<u8>> {
            mdo!{
                 let bytes = take_while1(is_digit);

                 ret bytes.iter().fold(0, |a, b| a * 10 + (b - b'0') as usize)
            }
        }

        let p = mdo!(
            let real = decimal();
                       char(b'.');
            let frac = decimal();

            ret _, Error<_> : (real, frac)
        );

        assert_eq!(p.parse(b"123.4567 "), State::Item(&[b' '], (123, 4567)));
    }

    #[test]
    fn mdo_closure() {
        fn decimal<'a>() -> impl Parser<'a, u8, usize, Error<u8>> {
            mdo!{
                 let bytes = take_while1(|c| c >= b'0' && c <= b'9');

                 ret bytes.iter().fold(0, |a, b| a * 10 + (b - b'0') as usize)
            }
        }

        let p = mdo!(
            let real = decimal();
                       satisfy(|c| c == b'.');
            let frac = decimal();

            ret _, Error<_> : (real, frac)
        );

        assert_eq!(p.parse(b"123.4567 "), State::Item(&[b' '], (123, 4567)));
    }
}
