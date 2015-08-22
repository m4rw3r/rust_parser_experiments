//! This is a private module to contain the smaller error type as well as adapters for using
//! it.
//! 
//! All adapters are #inline(always), and will just noop the data.
use std::marker::PhantomData;

#[derive(Debug, Eq, PartialEq)]
pub enum Error<I> {
    Unexpected,
    Many1(PhantomData<I>),
}

#[inline(always)]
pub fn expected<I>(_: I) -> Error<I> {
    Error::Unexpected
}

#[inline(always)]
pub fn string<I>(_: I) -> Error<I> {
    Error::Unexpected
}

#[inline(always)]
pub fn many1<I>() -> Error<I> {
    Error::Many1(PhantomData)
}
