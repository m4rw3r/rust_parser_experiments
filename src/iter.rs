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
pub struct Iter<'a, I: 'a + Copy, U, E, F>
  where F: FnMut(Input<'a, I>) -> Parser<'a, I, U, E> {
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
    pub fn new(buffer: &'a [I], f: F) -> Iter<'a, I, T, E, F> {
        Iter{
            l: IResult::Good,
            b: buffer,
            f: f,
            u: PhantomData,
            e: PhantomData
        }
    }

    pub fn last_state(&self) -> IResult {
        self.l
    }

    pub fn buffer(&self) -> &'a [I] {
        self.b
    }
}

impl<'a, I: 'a + Copy, T, E, F> Iterator for Iter<'a, I, T, E, F>
  where F: FnMut(Input<'a, I>) -> Parser<'a, I, T, E> {
    type Item = T;

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
