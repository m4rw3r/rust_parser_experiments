use std::marker::PhantomData;

use ::{
    Parser,
    State,
};

pub enum EndState<'a, I, E>
  where I: 'a {
    Error(&'a [I], E),
    Incomplete(usize),
}

/// Iterator used by ``many`` and ``many1``.
pub struct Iter<'a, I, U, E, F, P>
  where I: 'a,
        F: FnMut() -> P,
        P: Parser<'a, I, U, E> {
    state:  EndState<'a, I, E>,
    parser: F,
    buf:    &'a [I],
    _u:     PhantomData<U>,
}

impl<'a, I: 'a + Copy, T, E, F, P> Iter<'a, I, T, E, F, P>
  where F: FnMut() -> P,
        P: Parser<'a, I, T, E> {
    #[inline]
    pub fn new(buffer: &'a [I], parser: F) -> Iter<'a, I, T, E, F, P> {
        Iter{
            state:  EndState::Incomplete(0),
            parser: parser,
            buf:    buffer,
            _u:     PhantomData,
        }
    }

    /// Destructures the iterator returning the position just after the last successful parse as
    /// well as the state of the last attempt to parse data.
    #[inline]
    pub fn end_state(self) -> (&'a [I], EndState<'a, I, E>) {
        (self.buf, self.state)
    }
}

impl<'a, I, T, E, F, P> Iterator for Iter<'a, I, T, E, F, P>
  where I: 'a,
        F: FnMut() -> P,
        P: Parser<'a, I, T, E> {
    type Item = T;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        match (self.parser)().parse(self.buf) {
            State::Item(b, v) => {
                self.buf = b;

                Some(v)
            },
            State::Error(b, e) => {
                self.state = EndState::Error(b, e);

                None
            },
            State::Incomplete(n) => {
                self.state = EndState::Incomplete(n);

                None
            },
        }
    }
}
