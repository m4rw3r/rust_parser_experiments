use std::marker::PhantomData;

use ::{
    Input,
    Parser,
    State,
};

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
