pub trait Source<'a> {
    fn get(&mut self) -> Option<u8>;
    fn mark(&self) -> &'a [u8];
    fn restore(&mut self, &'a [u8]);
}

pub struct Slice<'a>(&'a [u8]);

impl<'a> Source<'a> for Slice<'a> {
    fn get(&mut self) -> Option<u8> {
        if self.0.is_empty() {
            None
        } else {
            let r = self.0[0];

            self.0 = &self.0[1..];

            Some(r)
        }
    }

    fn mark(&self) -> &'a [u8] {
        self.0
    }

    fn restore(&mut self, m: &'a [u8]) {
        self.0 = m
    }
}

pub trait Parser: Sized {
    type Result;

    fn parse(&self, &mut Source) -> Option<Self::Result>;

    fn or<O>(self, other: O) -> Or<Self, O, Self::Result>
      where O: Parser<Result=Self::Result> {
        Or(self, other)
    }

    fn and<O, F, R, T>(self, other: O, f: F) -> And<Self, O, F, Self::Result, R, T>
      where O: Parser<Result=R>,
            F: Fn(Self::Result, R) -> T {
        And(self, other, f)
    }

    /// Runs ``O`` after ``self`` provided ``self`` succeeds, discarding the result of ``self``,
    /// yielding the result of ``O``.
    ///
    /// If parsing fails no input will be consumed.
    fn then<O, R>(self, other: O) -> And<Self, O, fn(Self::Result, R) -> R, Self::Result, R, R>
      where O: Parser<Result=R> {
        /// Returns the second argument, discarding the first
        fn snd<A, B>(_: A, b: B) -> B {
            b
        }

        And(self, other, snd::<Self::Result, R>)
    }

    fn map<F, U>(self, f: F) -> Apply<Self, Self::Result, F, U>
      where F: Fn(Self::Result) -> U {
        Apply(self, f)
    }
}

/// Parser which succeeds on any input returning that input.
#[derive(Debug, Clone, Copy)]
pub struct Any;

impl Parser for Any {
    type Result = u8;

    fn parse(&self, i: &mut Source) -> Option<Self::Result> {
        i.get()
    }
}

/// Parser which succeeds on a specific character, returning that character.
#[derive(Debug, Clone, Copy)]
pub struct Char(u8);

impl Parser for Char {
    type Result = u8;

    fn parse(&self, i: &mut Source) -> Option<Self::Result> {
        i.get().and_then(|c| if c == self.0 { Some(c) } else { None })
    }
}

/// Parser which succeeds on a range of characters (inclusive), returning the matched character.
#[derive(Debug, Clone, Copy)]
pub struct Range(u8, u8);

impl Parser for Range {
    type Result = u8;

    fn parse(&self, i: &mut Source) -> Option<Self::Result> {
        i.get().and_then(|c| if self.0 <= c && c <= self.1 { Some(c) } else { None })
    }
}

/// Parser which succeeds if the character is present in a supplied list, returning the matched character.
#[derive(Debug, Clone, Copy)]
pub struct OneOf<'a>(&'a [u8]);

impl<'a> Parser for OneOf<'a> {
    type Result = u8;

    fn parse(&self, i: &mut Source) -> Option<Self::Result> {
        i.get().and_then(|c| if let Some(_) = self.0.iter().position(|&p| p == c) { Some(c) } else { None })
    }
}

/// Parser which fails if the nested parser succeeds, does not consume input,
/// returning ``R`` upon success.
#[derive(Debug, Clone, Copy)]
pub struct Not<P, R>(P, R)
  where P: Parser,
        R: Clone + Sized;

impl<P, R> Parser for Not<P, R>
  where P: Parser, R: Clone + Sized {
    type Result = R;

    fn parse(&self, i: &mut Source) -> Option<Self::Result> {
        let m = i.mark();
        
        match self.0.parse(i) {
            Some(_) => {
                i.restore(m);

                None
            },
            None    => {
                i.restore(m);

                Some(self.1.clone())
            }
        }
    }
}

/// Parser which returns the result of the nested parser on success otherwise ``R``.
#[derive(Debug, Clone, Copy)]
pub struct Maybe<P, R>(P, R)
  where P: Parser<Result=R>,
        R: Clone + Sized;

impl<P, R> Parser for Maybe<P, R>
  where P: Parser<Result=R>,
        R: Sized + Clone {
    type Result = R;

    fn parse(&self, i: &mut Source) -> Option<Self::Result> {
        let m = i.mark();

        match self.0.parse(i) {
            Some(r) => Some(r),
            None    => {
                i.restore(m);

                Some(self.1.clone())
            }
        }
    }
}

/// Parser which applies the function ``F`` on the result of the nested parser on success.
#[derive(Debug, Clone, Copy)]
pub struct Apply<P, R, F, U>(P, F)
  where P: Parser<Result=R>,
        F: Fn(R) -> U;

impl<P, R, F, U> Parser for Apply<P, R, F, U>
  where P: Parser<Result=R>,
        F: Fn(R) -> U {
    type Result = U;
    
    fn parse(&self, i: &mut Source) -> Option<Self::Result> {
        self.0.parse(i).map(&self.1)
    }
}

/// Parser which attempts to parse ``P`` zero or more times, using the fold
/// function ``F`` to accumulate data into the accumulator ``A``.
#[derive(Debug, Clone, Copy)]
pub struct Many<P, F, A, R>(P, F, A)
  where P: Parser<Result=R>,
        F: Fn(A, R) -> A,
        A: Clone + Sized;

impl<P, F, A, R> Parser for Many<P, F, A, R>
  where P: Parser<Result=R>,
        F: Fn(A, R) -> A,
        A: Clone + Sized {
    type Result = A;

    fn parse(&self, i: &mut Source) -> Option<Self::Result> {
        let mut r = self.2.clone();

        loop {
            let m = i.mark();

            match self.0.parse(i) {
                Some(v) => r = self.1(r, v),
                None    => {
                    i.restore(m);

                    break;
                }
            }
        }

        Some(r)
    }
}

/// Parser which attempts to parse ``P`` one or more times, using the fold
/// function ``F`` to accumulate data into the accumulator ``A``.
#[derive(Debug, Clone, Copy)]
pub struct Many1<P, F, A, R>(P, F, A)
  where P: Parser<Result=R>,
        F: Fn(A, R) -> A,
        A: Clone + Sized;

impl<P, F, A, R> Parser for Many1<P, F, A, R>
  where P: Parser<Result=R>,
        F: Fn(A, R) -> A,
        A: Clone + Sized {
    type Result = A;

    fn parse(&self, i: &mut Source) -> Option<Self::Result> {
        let mut n = 0;
        let mut r = self.2.clone();

        loop {
            let m = i.mark();

            match self.0.parse(i) {
                Some(v) => r = self.1(r, v),
                None    => {
                    i.restore(m);

                    break;
                }
            }

            n = n + 1;
        }

        if n > 0 {
            Some(r)
        } else {
            None
        }
    }
}

/// Parser which attempts to parse ``P`` exactly ``usize`` times, using the fold
/// function ``F`` to accumulate data into the accumulator ``A``.
#[derive(Debug, Clone, Copy)]
pub struct Count<P, F, A, R>(usize, P, F, A)
  where P: Parser<Result=R>,
        F: Fn(A, R) -> A,
        A: Clone + Sized;

impl<P, F, A, R> Parser for Count<P, F, A, R>
  where P: Parser<Result=R>,
        F: Fn(A, R) -> A,
        A: Clone + Sized {
    type Result = A;

    fn parse(&self, i: &mut Source) -> Option<Self::Result> {
        let mut n = 0;
        let mut r = self.3.clone();

        loop {
            let m = i.mark();

            match self.1.parse(i) {
                Some(v) => r = self.2(r, v),
                None    => {
                    i.restore(m);

                    break;
                }
            }

            n = n + 1;
        }

        if n == self.0 {
            Some(r)
        } else {
            None
        }
    }
}

/// Parser which first attempts ``A``, if ``A`` fails it will attempt ``B``. Returns the success
/// value of the first succeeding parser, otherwise it fails.
#[derive(Debug, Clone, Copy)]
pub struct Or<A, B, R>(A, B)
  where A: Parser<Result=R>,
        B: Parser<Result=R>;

impl<A, B, R> Parser for Or<A, B, R>
  where A: Parser<Result=R>,
        B: Parser<Result=R>{
    type Result = R;

    fn parse(&self, i: &mut Source) -> Option<Self::Result> {
        let m = i.mark();

        if let Some(x) = self.0.parse(i) {
            return Some(x);
        }

        i.restore(m);

        if let Some(x) = self.1.parse(i) {
            return Some(x);
        }

        i.restore(m);

        None
    }
}

#[derive(Debug, Clone, Copy)]
pub struct And<A, B, F, R, S, T>(A, B, F)
  where A: Parser<Result=R>,
        B: Parser<Result=S>,
        F: Fn(R, S) -> T;

impl<A, B, F, R, S, T> Parser for And<A, B, F, R, S, T>
  where A: Parser<Result=R>,
        B: Parser<Result=S>,
        F: Fn(R, S) -> T {
    type Result = T;

    fn parse(&self, i: &mut Source) -> Option<Self::Result> {
        let m = i.mark();

        if let Some(x) = self.0.parse(i) {
            if let Some(y) = self.1.parse(i) {
                return Some(self.2(x, y));
            }
        }

        i.restore(m);

        None
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_slice_get() {
        let mut s = Slice(b"abc");

        assert_eq!(s.get(), Some(b'a'));
        assert_eq!(s.get(), Some(b'b'));
        assert_eq!(s.get(), Some(b'c'));
        assert_eq!(s.get(), None);
    }

    #[test]
    fn test_any_empty() {
        let mut s = Slice(b"");

        assert_eq!(Any.parse(&mut s), None);
        assert_eq!(Any.parse(&mut s), None);
    }

    #[test]
    fn test_any() {
        let mut s = Slice(b"a");

        assert_eq!(Any.parse(&mut s), Some(b'a'));
        assert_eq!(Any.parse(&mut s), None);
    }

    #[test]
    fn test_parse_integer() {
        let mut s  = Slice(b"31415 ");
        let mut s2 = Slice(b" 31415 "); // should fail

        let p = Many1(Range(b'0', b'9'), |a: usize, x| a * 10 + (x - b'0') as usize, 0);

        assert_eq!(p.parse(&mut s2), None);
        assert_eq!(s2.get(), Some(b' '));
        assert_eq!(s2.get(), Some(b'3'));

        assert_eq!(p.parse(&mut s), Some(31415));
        assert_eq!(s.get(), Some(b' '));
    }

    #[test]
    fn test_or() {
        let mut s = Slice(b"123");

        let p = Char(b'1').or(Char(b'2'));

        assert_eq!(p.parse(&mut s), Some(b'1'));
        assert_eq!(p.parse(&mut s), Some(b'2'));
        assert_eq!(p.parse(&mut s), None);
        assert_eq!(s.get(), Some(b'3'));
    }

    #[test]
    fn test_neg_int() {
        let mut s  = Slice(b"123");
        let mut s2 = Slice(b"+123");
        let mut s3 = Slice(b"-123");
        let mut e  = Slice(b"-foo");

        fn num_accum(a: i32, x: u8) -> i32 {
            a * 10 + (x - b'0') as i32
        }

        let int = Many1(Range(b'0', b'9'), num_accum, 0);

        let plus  = Char(b'+').then(int);
        let minus = Char(b'-').and(int, |_, i| -i);
        let p     = plus.or(minus).or(int);

        assert_eq!(p.parse(&mut s), Some(123));
        assert_eq!(p.parse(&mut s2), Some(123));
        assert_eq!(p.parse(&mut s3), Some(-123));
        assert_eq!(p.parse(&mut e), None);
        assert_eq!(e.get(), Some(b'-'));
    }

    #[test]
    fn test_unicode() {
        let mut s = Slice("åäfÖ".as_bytes());

        fn sum(s: u32, c: u32) -> u32 {
            (s << 6) + c
        }

        fn chr(c: u8) -> u32 {
            (c & 0x3f) as u32
        }

        let tail   = Range(0b10000000, 0b10111111).map(chr);

        let ascii  = Range(0b00000000, 0b01111111).map(|c| c as u32);

        let mone   = Range(0b11000000, 0b11011111).map(|c| (c & 0b00011111) as u32).and(tail, sum);
        let mtwo   = Range(0b11100000, 0b11101111).map(|c| (c & 0b00001111) as u32).and(Count(2, tail, sum, 0), sum);
        let mthree = Range(0b11110000, 0b11110111).map(|c| (c & 0b00000111) as u32).and(Count(3, tail, sum, 0), sum);

        let utf8 = ascii.or(mone).or(mtwo).or(mthree);

        assert_eq!(utf8.parse(&mut s), Some(229)); // å
        assert_eq!(utf8.parse(&mut s), Some(228)); // ä
        assert_eq!(utf8.parse(&mut s), Some(102)); // f
        assert_eq!(utf8.parse(&mut s), Some(214)); // Ö
    }
}
