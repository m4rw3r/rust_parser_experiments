pub use self::wrap::{Iter, IResult};

#[cfg(not(feature = "verbose_error"))]
mod wrap {
    use std::marker::PhantomData;

    use ::{
        Input,
        Parser,
        State,
    };

    #[derive(Copy, Clone, Debug, Eq, PartialEq)]
    // TODO: Remove this from public
    pub enum IResult {
        Good,
        Bad,
        Incomplete,
    }

    // TODO: Needs some better way of iterating items
    pub struct Iter<'a, I, U, E, F>
      where I: 'a,
            F: FnMut(Input<'a, I>) -> Parser<'a, I, U, E> {
        /// Last resulting state
        l: IResult,
        /// Current buffer
        b: &'a [I],
        f: F,
        u: PhantomData<U>,
        e: PhantomData<E>,
    }

    impl<'a, I: 'a + Copy, T, E, F> Iter<'a, I, T, E, F>
      where F: FnMut(Input<'a, I>) -> Parser<'a, I, T, E> {
        #[inline]
        pub fn new(buffer: &'a [I], f: F) -> Iter<'a, I, T, E, F> {
            Iter{
                l: IResult::Good,
                b: buffer,
                f: f,
                u: PhantomData,
                e: PhantomData
            }
        }

        #[inline]
        pub fn last_state(&self) -> IResult {
            self.l
        }

        #[inline]
        pub fn buffer(&self) -> &'a [I] {
            self.b
        }
    }

    impl<'a, I: 'a + Copy, T, E, F> Iterator for Iter<'a, I, T, E, F>
      where F: FnMut(Input<'a, I>) -> Parser<'a, I, T, E> {
        type Item = T;

        #[inline]
        fn next(&mut self) -> Option<Self::Item> {
            if self.l != IResult::Good {
                return None
            }

            match (self.f)(Input(self.b)).0 {
                State::Item(b, v)    => {
                    self.l = IResult::Good;
                    self.b = b;

                    Some(v)
                },
                State::Error(_, _)   => { self.l = IResult::Bad;        None },
                State::Incomplete(_) => { self.l = IResult::Incomplete; None },
            }
        }
    }
}

#[cfg(feature = "verbose_error")]
mod wrap {
    use std::marker::PhantomData;

    use ::{
        Input,
        Parser,
        State,
    };

    pub struct IResult;

    pub struct Iter<'a, I, U, E, F>
      where I: 'a,
            F: FnMut(Input<'a, I>) -> Parser<'a, I, U, E> {
        s: State<'a, I, (), E>,
        f: F,
        b: &'a [I],
        u: PhantomData<U>,
    }

    impl<'a, I: 'a + Copy, T, E, F> Iter<'a, I, T, E, F>
      where F: FnMut(Input<'a, I>) -> Parser<'a, I, T, E> {
        #[inline]
        pub fn new(buffer: &'a [I], f: F) -> Iter<'a, I, T, E, F> {
            Iter{
                s: State::Item(buffer, ()),
                f: f,
                b: buffer,
                u: PhantomData,
            }
        }

        #[inline]
        pub fn state(self) -> (&'a [I], State<'a, I, (), E>) {
            (self.b, self.s)
        }
    }

    impl<'a, I, T, E, F> Iterator for Iter<'a, I, T, E, F>
      where I: 'a,
            F: FnMut(Input<'a, I>) -> Parser<'a, I, T, E> {
        type Item = T;

        #[inline]
        fn next(&mut self) -> Option<Self::Item> {
            match (self.f)(Input(self.b)) {
                Parser(State::Item(b, v)) => {
                    self.b = b;
                    self.s = State::Item(b, ());

                    Some(v)
                },
                Parser(State::Error(b, e)) => {
                    self.s = State::Error(b, e);

                    None
                },
                Parser(State::Incomplete(n)) => {
                    self.s = State::Incomplete(n);

                    None
                },
            }
        }
    }
}
