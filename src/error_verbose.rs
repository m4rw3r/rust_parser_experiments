//! This is a private module to contain the smaller error type as well as adapters for using
//! it.
//! 
//! All adapters are #inline(always) and will construct the appropriate error type.
use std::error;

#[derive(Debug)]
pub enum Error<'a, I>
  where I: 'a {
    Expected(I),
    Unexpected,
    Many1(ErrorPosition<'a, I, Box<error::Error>>),
}

#[inline(always)]
pub fn expected<I>(i: I) -> Error<I> {
    Error::Expected(i)
}

#[derive(Debug)]
pub struct ErrorPosition<'a, I, E>(&'a [I], E)
  where I: 'a,
        E: Sized;

#[inline(always)]
pub fn string<I>(c: I) -> Error<I> {
    Error::Expected(c)
}

#[inline(always)]
pub fn many1<I>() -> Error<I> {
    Error::Many1(PhantomData)
}
