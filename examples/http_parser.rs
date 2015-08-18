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

#[macro_use]
extern crate parser;

use parser::*;

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

fn end_of_line<'a>() -> Parser<'a, 'a, u8, u8, Error<u8>> {
    or(mdo!{
        char(b'\r');
        char(b'\n');

        ret u8, Error<u8>: b'\r'
    }, char(b'\n'))
}

fn http_version<'a>() -> Parser<'a, 'a, u8, &'a [u8], Error<u8>> {
    mdo!{
                      string(b"HTTP/");
        let version = take_while1(is_http_version);

        ret version
    }
}

fn request_line<'a>() -> Parser<'a, 'a, u8, Request<'a>, Error<u8>> {
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

fn message_header_line<'a>() -> Parser<'a, 'a, u8, &'a [u8], Error<u8>> {
    mdo!{
               take_while1(is_horizontal_space);
        let line = take_till(is_end_of_line);
               end_of_line();

        ret line
    }
}

fn message_header<'a>() -> Parser<'a, 'a, u8, Header<'a>, Error<u8>> {
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

fn request<'a>() -> Parser<'a, 'a, u8, (Request<'a>, Vec<Header<'a>>), Error<u8>> {
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

    let i = parser::iter::Iter::new(&contents, request);

    println!("num: {}", i.count());
}
