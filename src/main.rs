//! http parser comparable to the http-parser found in attoparsec's examples.
//! 
//! Reads data in the following format:
//! 
//! ```text
//! GET /robot.txt HTTP/1.1
//! Host: localhost
//! Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8
//! 
//! ```

#![feature(default_type_parameter_fallback)]

use std::fmt;

mod iter;

pub mod combinators;
pub mod monad;
pub mod parsers;
#[macro_use]
pub mod mdo;

pub use monad::{
    bind,
    ret,
    err,
};

pub use combinators::{
    count,
    option,
    or,
    many,
    many1,
};

pub use parsers::{
    any,
    char,
    satisfy,
    take_while,
    take_while1,
    take_till,
    string,
};

pub use error::Error;

pub trait Parser<'a, I, T, E> {
    fn parse(self, &'a [I]) -> State<'a, I, T, E>;
}

impl<'a, I, T, E, F> Parser<'a, I, T, E> for F
  where F: FnOnce(&'a [I]) -> State<'a, I, T, E> {
    fn parse(self, i: &'a [I]) -> State<'a, I, T, E> {
        self(i)
    }
}

/// Constructs a parser from a closure.
/// 
/// This function is necessary for inference at time.
fn parser<'a, 'p, I, T, E, F>(p: F) -> impl Parser<'a, I, T, E> + 'p
  where F: FnOnce(&'a [I]) -> State<'a, I, T, E> + 'p {
    p
}

#[derive(Debug, Eq, PartialEq)]
#[must_use]
pub enum State<'a, I, T, E>
  where I: 'a {
    /// Success state, slice is the slice of the input data starting just after the last successful
    /// parser.
    Item(&'a [I], T),
    /// Error state, slice is the slice of the input data from the position where the error
    /// occurred.
    Error(&'a [I], E),
    // Do not include data-slice here as we do not need to actually return the part which is
    // incomplete. The parse has to restart anyway from the index where it was.
    Incomplete(usize),
}

impl<'a, I, T, E> State<'a, I, T, E>
  where T: fmt::Debug {
    pub fn unwrap_err(self) -> E {
        match self {
            State::Item(_, t)    => panic!("called `Parser::unwrap_err` on a parser in a success state: {:?}", t),
            State::Error(_, e)   => e,
            State::Incomplete(_) => panic!("called `Parser::unwrap_err` on a parser in an incomplete state"),
        }
    }
}

impl<'a, I, T, E> State<'a, I, T, E>
  where E: fmt::Debug {
    pub fn unwrap(self) -> T {
        match self {
            State::Item(_, t)    => t,
            State::Error(_, e)   => panic!("called `Parser::unwrap` on a parser in an error state: {:?}", e),
            State::Incomplete(_) => panic!("called `Parser::unwrap` on a parser in an incomplete state"),
        }
    }
}

pub struct Iter<'a, I, U, E, F, P>(iter::Iter<'a, I, U, E, F, P>)
  where I: 'a,
        F: FnMut() -> P,
        P: Parser<'a, I, U, E>;

impl<'a, I: 'a + Copy, T, E, F, P> Iter<'a, I, T, E, F, P>
  where F: FnMut() -> P,
        P: Parser<'a, I, T, E> {
    #[inline]
    pub fn new(buffer: &'a [I], f: F) -> Self {
        Iter(iter::Iter::new(buffer, f))
    }
}

impl<'a, I, T, E, F, P> Iterator for Iter<'a, I, T, E, F, P>
  where I: 'a,
        F: FnMut() -> P,
        P: Parser<'a, I, T, E> {
    type Item = T;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.0.next()
    }
}

#[cfg(feature = "verbose_error")]
mod error {
    //! This is a private module to contain the more verbose error type as well as adapters for
    //! using it.
    //! 
    //! All adapters are #inline(always) and will construct the appropriate error type.
    use std::fmt;

    use ::State;

    #[derive(Debug, Eq, PartialEq)]
    pub enum Error<I> {
        Expected(I),
        Unexpected,
        String(Vec<I>),
    }

    impl<I> fmt::Display for Error<I>
      where I: fmt::Debug {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            match *self {
                Error::Expected(ref c) => write!(f, "expected {:?}", *c),
                Error::Unexpected      => write!(f, "unexpected"),
                Error::String(ref s)   => write!(f, "expected {:?}", *s),
            }
        }
    }

    #[inline(always)]
    pub fn unexpected<I>() -> Error<I> {
        Error::Unexpected
    }

    #[inline(always)]
    pub fn expected<'a, I>(i: I) -> Error<I> {
        Error::Expected(i)
    }


    #[inline(always)]
    pub fn string<'a, 'b, I, T>(buffer: &'a [I], _offset: usize, expected: &'b [I]) -> State<'a, I, T, Error<I>>
      where I: Copy {
        State::Error(buffer, Error::String(expected.to_vec()))
    }
}

#[cfg(not(feature = "verbose_error"))]
mod error {
    //! This is a private module to contain the smaller error type as well as adapters for using
    //! it.
    //! 
    //! All adapters are #inline(always), and will just noop the data.
    use std::marker::PhantomData;

    use ::State;

    #[derive(Debug, Eq, PartialEq)]
    pub struct Error<I>(PhantomData<I>);

    #[inline(always)]
    pub fn unexpected<I>() -> Error<I> {
        Error(PhantomData)
    }

    #[inline(always)]
    pub fn expected<'a, I>(_: I) -> Error<I> {
        Error(PhantomData)
    }

    #[inline(always)]
    pub fn string<'a, 'b, I, T>(buffer: &'a [I], offset: usize, _expected: &'b [I]) -> State<'a, I, T, Error<I>>
      where I: Copy {
        State::Error(&buffer[offset..], Error(PhantomData))
    }
}

use std::fs::File;
use std::env;

#[derive(Debug)]
struct Request<'a> {
    method:  &'a [u8],
    uri:     &'a [u8],
    version: &'a [u8],
}

#[derive(Debug)]
struct Header<'a> {
    name:  &'a [u8],
    value: Vec<&'a [u8]>,
}


fn is_token(c: u8) -> bool {
    c < 128 && c > 31 && b"()<>@,;:\\\"/[]?={} \t".iter().position(|&i| i == c).is_none()
}

fn is_horizontal_space(c: u8) -> bool { c == b' ' || c == b'\t' }
fn is_space(c: u8)            -> bool { c == b' ' }
fn is_not_space(c: u8)        -> bool { c != b' ' }
fn is_end_of_line(c: u8)      -> bool { c == b'\r' || c == b'\n' }
fn is_http_version(c: u8)     -> bool { c >= b'0' && c <= b'9' || c == b'.' }

fn end_of_line<'a>() -> impl Parser<'a, u8, u8, Error<u8>> {
    or(mdo!{
        char(b'\r');
        char(b'\n');

        ret u8, Error<u8>: b'\r'
    }, char(b'\n'))
}

fn http_version<'a>() -> impl Parser<'a, u8, &'a [u8], Error<u8>> {
    mdo!{
                      string(b"HTTP/");
        let version = take_while1(is_http_version);

        ret version
    }
}

#[inline(always)]
fn request_line<'a>() -> impl Parser<'a, u8, Request<'a>, Error<u8>> {
    mdo!{
        let method  = take_while1(is_token);
                      take_while1(is_space);
        let uri     = take_while1(is_not_space);
                      take_while1(is_space);
        let version = http_version();

        ret Request {
            method:  method,
            uri:     uri,
            version: version,
        }
    }
}

fn message_header_line<'a>() -> impl Parser<'a, u8, &'a [u8], Error<u8>> {
    mdo!{
                   take_while1(is_horizontal_space);
        let line = take_till(is_end_of_line);
                   end_of_line();

        ret line
    }
}

fn message_header<'a>() -> impl Parser<'a, u8, Header<'a>, Error<u8>> {
    mdo!{
        let name  = take_while1(is_token);
                    char(b':');
        let lines = many1(message_header_line);

        ret Header {
            name:  name,
            value: lines,
        }
    }
}

#[inline(always)]
fn request<'a>() -> impl Parser<'a, u8, (Request<'a>, Vec<Header<'a>>), Error<u8>> {
    mdo!{
        let r = request_line();
                end_of_line();
        let h = many(message_header);
                end_of_line();

        ret (r, h)
    }
}

fn main() {
    let mut contents: Vec<u8> = Vec::new();

    {
        use std::io::Read;

        let mut file = File::open(env::args().nth(1).expect("File to read")).ok().expect("Failed to open file");

        let _ = file.read_to_end(&mut contents).unwrap();
    }

    let i = Iter::new(&contents, request);

    println!("num: {}", i.count());
}
