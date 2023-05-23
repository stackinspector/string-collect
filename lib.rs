mod utf8 {
    mod lossy {
        use super::*;

        /// A push-based, lossy decoder for UTF-8.
        /// Errors are replaced with the U+FFFD replacement character.
        ///
        /// Users “push” bytes into the decoder, which in turn “pushes” `&str` slices into a callback.
        ///
        /// For example, `String::from_utf8_lossy` (but returning `String` instead of `Cow`)
        /// can be rewritten as:
        ///
        /// ```rust
        /// fn string_from_utf8_lossy(input: &[u8]) -> String {
        ///     let mut string = String::new();
        ///     utf8::LossyDecoder::new(|s| string.push_str(s)).feed(input);
        ///     string
        /// }
        /// ```
        ///
        /// **Note:** Dropping the decoder signals the end of the input:
        /// If the last input chunk ended with an incomplete byte sequence for a code point,
        /// this is an error and a replacement character is emitted.
        /// Use `std::mem::forget` to inhibit this behavior.
        pub struct LossyDecoder<F: FnMut(&str)> {
            push_str: F,
            incomplete: Incomplete,
        }
        
        impl<F: FnMut(&str)> LossyDecoder<F> {
            /// Create a new decoder from a callback.
            #[inline]
            pub fn new(push_str: F) -> Self {
                LossyDecoder {
                    push_str: push_str,
                    incomplete: Incomplete {
                        buffer: [0, 0, 0, 0],
                        buffer_len: 0,
                    },
                }
            }
        
            /// Feed one chunk of input into the decoder.
            ///
            /// The input is decoded lossily
            /// and the callback called once or more with `&str` string slices.
            ///
            /// If the UTF-8 byte sequence for one code point was split into this bytes chunk
            /// and previous bytes chunks, it will be correctly pieced back together.
            pub fn feed(&mut self, mut input: &[u8]) {
                if self.incomplete.buffer_len > 0 {
                    match self.incomplete.try_complete(input) {
                        Some((Ok(s), remaining)) => {
                            (self.push_str)(s);
                            input = remaining
                        }
                        Some((Err(_), remaining)) => {
                            (self.push_str)(REPLACEMENT_CHARACTER);
                            input = remaining
                        }
                        None => {
                            return
                        }
                    }
                }
                loop {
                    match decode(input) {
                        Ok(s) => {
                            (self.push_str)(s);
                            return
                        }
                        Err(DecodeError::Incomplete { valid_prefix, incomplete_suffix }) => {
                            (self.push_str)(valid_prefix);
                            self.incomplete = incomplete_suffix;
                            return
                        }
                        Err(DecodeError::Invalid { valid_prefix, remaining_input, .. }) => {
                            (self.push_str)(valid_prefix);
                            (self.push_str)(REPLACEMENT_CHARACTER);
                            input = remaining_input
                        }
                    }
                }
            }
        }
        
        impl<F: FnMut(&str)> Drop for LossyDecoder<F> {
            #[inline]
            fn drop(&mut self) {
                if self.incomplete.buffer_len > 0 {
                    (self.push_str)(REPLACEMENT_CHARACTER)
                }
            }
        }
    }
    mod read {
        use std::io::{self, BufRead};
        use std::error::Error;
        use std::fmt;
        use std::str;
        use super::*;
        
        /// Wraps a `std::io::BufRead` buffered byte stream and decode it as UTF-8.
        pub struct BufReadDecoder<B: BufRead> {
            buf_read: B,
            bytes_consumed: usize,
            incomplete: Incomplete,
        }
        
        #[derive(Debug)]
        pub enum BufReadDecoderError<'a> {
            /// Represents one UTF-8 error in the byte stream.
            ///
            /// In lossy decoding, each such error should be replaced with U+FFFD.
            /// (See `BufReadDecoder::next_lossy` and `BufReadDecoderError::lossy`.)
            InvalidByteSequence(&'a [u8]),
        
            /// An I/O error from the underlying byte stream
            Io(io::Error),
        }
        
        impl<'a> BufReadDecoderError<'a> {
            /// Replace UTF-8 errors with U+FFFD
            pub fn lossy(self) -> Result<&'static str, io::Error> {
                match self {
                    BufReadDecoderError::Io(error) => Err(error),
                    BufReadDecoderError::InvalidByteSequence(_) => Ok(REPLACEMENT_CHARACTER),
                }
            }
        }
        
        impl<'a> fmt::Display for BufReadDecoderError<'a> {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                match *self {
                    BufReadDecoderError::InvalidByteSequence(bytes) => {
                        write!(f, "invalid byte sequence: {:02x?}", bytes)
                    }
                    BufReadDecoderError::Io(ref err) => write!(f, "underlying bytestream error: {}", err),
                }
            }
        }
        
        impl<'a> Error for BufReadDecoderError<'a> {
            fn source(&self) -> Option<&(dyn Error + 'static)> {
                match *self {
                    BufReadDecoderError::InvalidByteSequence(_) => None,
                    BufReadDecoderError::Io(ref err) => Some(err),
                }
            }
        }
        
        impl<B: BufRead> BufReadDecoder<B> {
            /// This is to `Read::read_to_string` what `String::from_utf8_lossy` is to `String::from_utf8`.
            pub fn read_to_string_lossy(buf_read: B) -> io::Result<String> {
                let mut decoder = Self::new(buf_read);
                let mut string = String::new();
                while let Some(result) = decoder.next_lossy() {
                    string.push_str(result?)
                }
                Ok(string)
            }
        
            pub fn new(buf_read: B) -> Self {
                Self {
                    buf_read,
                    bytes_consumed: 0,
                    incomplete: Incomplete::empty(),
                }
            }
        
            /// Same as `BufReadDecoder::next_strict`, but replace UTF-8 errors with U+FFFD.
            pub fn next_lossy(&mut self) -> Option<io::Result<&str>> {
                self.next_strict().map(|result| result.or_else(|e| e.lossy()))
            }
        
            /// Decode and consume the next chunk of UTF-8 input.
            ///
            /// This method is intended to be called repeatedly until it returns `None`,
            /// which represents EOF from the underlying byte stream.
            /// This is similar to `Iterator::next`,
            /// except that decoded chunks borrow the decoder (~iterator)
            /// so they need to be handled or copied before the next chunk can start decoding.
            pub fn next_strict(&mut self) -> Option<Result<&str, BufReadDecoderError>> {
                enum BytesSource {
                    BufRead(usize),
                    Incomplete,
                }
                macro_rules! try_io {
                    ($io_result: expr) => {
                        match $io_result {
                            Ok(value) => value,
                            Err(error) => return Some(Err(BufReadDecoderError::Io(error)))
                        }
                    }
                }
                let (source, result) = loop {
                    if self.bytes_consumed > 0 {
                        self.buf_read.consume(self.bytes_consumed);
                        self.bytes_consumed = 0;
                    }
                    let buf = try_io!(self.buf_read.fill_buf());
        
                    // Force loop iteration to go through an explicit `continue`
                    enum Unreachable {}
                    let _: Unreachable = if self.incomplete.is_empty() {
                        if buf.is_empty() {
                            return None  // EOF
                        }
                        match str::from_utf8(buf) {
                            Ok(_) => {
                                break (BytesSource::BufRead(buf.len()), Ok(()))
                            }
                            Err(error) => {
                                let valid_up_to = error.valid_up_to();
                                if valid_up_to > 0 {
                                    break (BytesSource::BufRead(valid_up_to), Ok(()))
                                }
                                match error.error_len() {
                                    Some(invalid_sequence_length) => {
                                        break (BytesSource::BufRead(invalid_sequence_length), Err(()))
                                    }
                                    None => {
                                        self.bytes_consumed = buf.len();
                                        self.incomplete = Incomplete::new(buf);
                                        // need more input bytes
                                        continue
                                    }
                                }
                            }
                        }
                    } else {
                        if buf.is_empty() {
                            break (BytesSource::Incomplete, Err(()))  // EOF with incomplete code point
                        }
                        let (consumed, opt_result) = self.incomplete.try_complete_offsets(buf);
                        self.bytes_consumed = consumed;
                        match opt_result {
                            None => {
                                // need more input bytes
                                continue
                            }
                            Some(result) => {
                                break (BytesSource::Incomplete, result)
                            }
                        }
                    };
                };
                let bytes = match source {
                    BytesSource::BufRead(byte_count) => {
                        self.bytes_consumed = byte_count;
                        let buf = try_io!(self.buf_read.fill_buf());
                        &buf[..byte_count]
                    }
                    BytesSource::Incomplete => {
                        self.incomplete.take_buffer()
                    }
                };
                match result {
                    Ok(()) => Some(Ok(unsafe { str::from_utf8_unchecked(bytes) })),
                    Err(()) => Some(Err(BufReadDecoderError::InvalidByteSequence(bytes))),
                }
            }
        }
    }
    
    pub use lossy::LossyDecoder;
    pub use read::{BufReadDecoder, BufReadDecoderError};
    
    use std::cmp;
    use std::error::Error;
    use std::fmt;
    use std::str;
    
    /// The replacement character, U+FFFD. In lossy decoding, insert it for every decoding error.
    pub const REPLACEMENT_CHARACTER: &'static str = "\u{FFFD}";
    
    #[derive(Debug, Copy, Clone)]
    pub enum DecodeError<'a> {
        /// In lossy decoding insert `valid_prefix`, then `"\u{FFFD}"`,
        /// then call `decode()` again with `remaining_input`.
        Invalid {
            valid_prefix: &'a str,
            invalid_sequence: &'a [u8],
            remaining_input: &'a [u8],
        },
    
        /// Call the `incomplete_suffix.try_complete` method with more input when available.
        /// If no more input is available, this is an invalid byte sequence.
        Incomplete {
            valid_prefix: &'a str,
            incomplete_suffix: Incomplete,
        },
    }
    
    impl<'a> fmt::Display for DecodeError<'a> {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            match *self {
                DecodeError::Invalid {
                    valid_prefix,
                    invalid_sequence,
                    remaining_input,
                } => write!(
                    f,
                    "found invalid byte sequence {invalid_sequence:02x?} after \
                     {valid_byte_count} valid bytes, followed by {unprocessed_byte_count} more \
                     unprocessed bytes",
                    invalid_sequence = invalid_sequence,
                    valid_byte_count = valid_prefix.len(),
                    unprocessed_byte_count = remaining_input.len()
                ),
                DecodeError::Incomplete {
                    valid_prefix,
                    incomplete_suffix,
                } => write!(
                    f,
                    "found incomplete byte sequence {incomplete_suffix:02x?} after \
                     {valid_byte_count} bytes",
                    incomplete_suffix = incomplete_suffix,
                    valid_byte_count = valid_prefix.len()
                ),
            }
        }
    }
    
    impl<'a> Error for DecodeError<'a> {}
    
    #[derive(Debug, Copy, Clone)]
    pub struct Incomplete {
        pub buffer: [u8; 4],
        pub buffer_len: u8,
    }
    
    pub fn decode(input: &[u8]) -> Result<&str, DecodeError> {
        let error = match str::from_utf8(input) {
            Ok(valid) => return Ok(valid),
            Err(error) => error,
        };
    
        // FIXME: separate function from here to guide inlining?
        let (valid, after_valid) = input.split_at(error.valid_up_to());
        let valid = unsafe {
            str::from_utf8_unchecked(valid)
        };
    
        match error.error_len() {
            Some(invalid_sequence_length) => {
                let (invalid, rest) = after_valid.split_at(invalid_sequence_length);
                Err(DecodeError::Invalid {
                    valid_prefix: valid,
                    invalid_sequence: invalid,
                    remaining_input: rest
                })
            }
            None => {
                Err(DecodeError::Incomplete {
                    valid_prefix: valid,
                    incomplete_suffix: Incomplete::new(after_valid),
                })
            }
        }
    }
    
    impl Incomplete {
        pub fn empty() -> Self {
            Incomplete {
                buffer: [0, 0, 0, 0],
                buffer_len: 0,
            }
        }
    
        pub fn is_empty(&self) -> bool {
            self.buffer_len == 0
        }
    
        pub fn new(bytes: &[u8]) -> Self {
            let mut buffer = [0, 0, 0, 0];
            let len = bytes.len();
            buffer[..len].copy_from_slice(bytes);
            Incomplete {
                buffer: buffer,
                buffer_len: len as u8,
            }
        }
    
        /// * `None`: still incomplete, call `try_complete` again with more input.
        ///   If no more input is available, this is invalid byte sequence.
        /// * `Some((result, remaining_input))`: We’re done with this `Incomplete`.
        ///   To keep decoding, pass `remaining_input` to `decode()`.
        pub fn try_complete<'input>(&mut self, input: &'input [u8])
                                    -> Option<(Result<&str, &[u8]>, &'input [u8])> {
            let (consumed, opt_result) = self.try_complete_offsets(input);
            let result = opt_result?;
            let remaining_input = &input[consumed..];
            let result_bytes = self.take_buffer();
            let result = match result {
                Ok(()) => Ok(unsafe { str::from_utf8_unchecked(result_bytes) }),
                Err(()) => Err(result_bytes),
            };
            Some((result, remaining_input))
        }
    
        fn take_buffer(&mut self) -> &[u8] {
            let len = self.buffer_len as usize;
            self.buffer_len = 0;
            &self.buffer[..len as usize]
        }
    
        /// (consumed_from_input, None): not enough input
        /// (consumed_from_input, Some(Err(()))): error bytes in buffer
        /// (consumed_from_input, Some(Ok(()))): UTF-8 string in buffer
        fn try_complete_offsets(&mut self, input: &[u8]) -> (usize, Option<Result<(), ()>>) {
            let initial_buffer_len = self.buffer_len as usize;
            let copied_from_input;
            {
                let unwritten = &mut self.buffer[initial_buffer_len..];
                copied_from_input = cmp::min(unwritten.len(), input.len());
                unwritten[..copied_from_input].copy_from_slice(&input[..copied_from_input]);
            }
            let spliced = &self.buffer[..initial_buffer_len + copied_from_input];
            match str::from_utf8(spliced) {
                Ok(_) => {
                    self.buffer_len = spliced.len() as u8;
                    (copied_from_input, Some(Ok(())))
                }
                Err(error) => {
                    let valid_up_to = error.valid_up_to();
                    if valid_up_to > 0 {
                        let consumed = valid_up_to.checked_sub(initial_buffer_len).unwrap();
                        self.buffer_len = valid_up_to as u8;
                        (consumed, Some(Ok(())))
                    } else {
                        match error.error_len() {
                            Some(invalid_sequence_length) => {
                                let consumed = invalid_sequence_length
                                    .checked_sub(initial_buffer_len).unwrap();
                                self.buffer_len = invalid_sequence_length as u8;
                                (consumed, Some(Err(())))
                            }
                            None => {
                                self.buffer_len = spliced.len() as u8;
                                (copied_from_input, None)
                            }
                        }
                    }
                }
            }
        }
    }    
}

use utf8::DecodeError;

#[derive(Debug)]
pub struct StringCollector {
    data: String,
    incomplete: Option<utf8::Incomplete>,
}

impl StringCollector {
    pub fn new() -> Self {
        StringCollector { data: String::new(), incomplete: None }
    }

    pub fn len(&self) -> usize {
        self.data
            .len()
            .saturating_add(self.incomplete.map(|i| i.buffer_len as usize).unwrap_or(0))
    }

    pub fn extend<T: AsRef<[u8]>>(&mut self, tail: T) -> Result<(), ()> {
        let mut input: &[u8] = tail.as_ref();

        if let Some(mut incomplete) = self.incomplete.take() {
            if let Some((result, rest)) = incomplete.try_complete(input) {
                input = rest;
                if let Ok(text) = result {
                    self.data.push_str(text);
                } else {
                    return Err(());
                }
            } else {
                input = &[];
                self.incomplete = Some(incomplete);
            }
        }

        if !input.is_empty() {
            match utf8::decode(input) {
                Ok(text) => {
                    self.data.push_str(text);
                    Ok(())
                }
                Err(DecodeError::Incomplete { valid_prefix, incomplete_suffix }) => {
                    self.data.push_str(valid_prefix);
                    self.incomplete = Some(incomplete_suffix);
                    Ok(())
                }
                Err(DecodeError::Invalid { valid_prefix, .. }) => {
                    self.data.push_str(valid_prefix);
                    Err(())
                }
            }
        } else {
            Ok(())
        }
    }

    pub fn into_string(self) -> Result<String, ()> {
        if self.incomplete.is_some() {
            Err(())
        } else {
            Ok(self.data)
        }
    }
}
