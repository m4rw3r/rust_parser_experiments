use std::marker::PhantomData;

use ::{
    Input,
    Parser,
    State,
};

/// Iterator used by ``many`` and ``many1``.
pub struct Iter<'a, I, U, E, F>
  where I: 'a,
        F: FnMut(Input<'a, I>) -> Parser<'a, I, U, E> {
    state:  State<'a, I, (), E>,
    parser: F,
    buf:    &'a [I],
    _u:     PhantomData<U>,
}

impl<'a, I: 'a + Copy, T, E, F> Iter<'a, I, T, E, F>
  where F: FnMut(Input<'a, I>) -> Parser<'a, I, T, E> {
    #[inline]
    pub fn new(buffer: &'a [I], parser: F) -> Iter<'a, I, T, E, F> {
        Iter{
            state:  State::Item(buffer, ()),
            parser: parser,
            buf:    buffer,
            _u:     PhantomData,
        }
    }

    /// Destructures the iterator returning the position just after the last successful parse as
    /// well as the state of the last attempt to parse data.
    #[inline]
    pub fn state(self) -> (&'a [I], State<'a, I, (), E>) {
        (self.buf, self.state)
    }
}

impl<'a, I, T, E, F> Iterator for Iter<'a, I, T, E, F>
  where I: 'a,
        F: FnMut(Input<'a, I>) -> Parser<'a, I, T, E> {
    type Item = T;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        match (self.parser)(Input(self.buf)) {
            Parser(State::Item(b, v)) => {
                self.buf   = b;
                self.state = State::Item(b, ());

                Some(v)
            },
            Parser(State::Error(b, e)) => {
                self.state = State::Error(b, e);

                None
            },
            Parser(State::Incomplete(n)) => {
                self.state = State::Incomplete(n);

                None
            },
        }
    }
}
