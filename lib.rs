use std::{cmp, str, borrow::Cow};

#[derive(Debug, Clone)]
pub struct DecodeError<'input> {
    pub invalid_sequence: Cow<'input, [u8]>,
    pub remaining_input: &'input [u8],
}

impl<'a> std::fmt::Display for DecodeError<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "found invalid byte sequence {:02x?} followed by {} more unprocessed bytes",
            self.invalid_sequence,
            self.remaining_input.len(),
        )
    }
}

impl<'a> std::error::Error for DecodeError<'a> {}

#[derive(Debug, Copy, Clone)]
pub struct Incomplete {
    pub buffer: [u8; 4],
    pub buffer_len: u8,
}

impl Incomplete {
    pub fn new(bytes: &[u8]) -> Self {
        let mut buffer = [0, 0, 0, 0];
        let len = bytes.len();
        buffer[..len].copy_from_slice(bytes);
        Incomplete {
            buffer,
            buffer_len: len as u8,
        }
    }

    /// * `None`: still incomplete, call `try_complete` again with more input.
    ///   If no more input is available, this is invalid byte sequence.
    /// * `Some((result, remaining_input))`: We’re done with this `Incomplete`.
    ///   To keep decoding, pass `remaining_input` to `decode()`.
    pub fn try_complete<'input>(&mut self, input: &'input [u8]) -> Option<(Result<&str, &[u8]>, &'input [u8])> {
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
        let len = self.buffer_len;
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

#[derive(Debug)]
pub struct StringCollector {
    data: String,
    incomplete: Option<Incomplete>,
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

    pub fn extend<'input>(&mut self, mut input: &'input [u8]) -> Result<(), DecodeError<'input>> {
        if let Some(mut incomplete) = self.incomplete.take() {
            if let Some((result, rest)) = incomplete.try_complete(input) {
                match result {
                    Ok(vaild) => {
                        self.data.push_str(vaild);
                        input = rest;
                    }
                    Err(invalid) => {
                        return Err(DecodeError {
                            // TODO array for invalid data from previous
                            invalid_sequence: Cow::Owned(invalid.to_vec()),
                            remaining_input: rest
                        });
                    }
                }
            } else {
                input = &[];
                self.incomplete = Some(incomplete);
            }
        }

        if !input.is_empty() {
            match str::from_utf8(input) {
                Ok(valid) => {
                    self.data.push_str(valid);
                    Ok(())
                },
                Err(error) => {
                    let (valid, after_valid) = input.split_at(error.valid_up_to());
                    self.data.push_str(unsafe { str::from_utf8_unchecked(valid) });

                    match error.error_len() {
                        Some(invalid_sequence_length) => {
                            let (invalid, remaining_input) = after_valid.split_at(invalid_sequence_length);
                            Err(DecodeError {
                                invalid_sequence: Cow::Borrowed(invalid),
                                remaining_input
                            })
                        }
                        None => {
                            self.incomplete = Some(Incomplete::new(after_valid));
                            Ok(())
                        }
                    }
                },
            }
        } else {
            Ok(())
        }
    }

    pub fn into_string(self) -> Result<String, (String, Incomplete)> {
        match self.incomplete {
            None => Ok(self.data),
            Some(incomplete) => Err((self.data, incomplete)),
        }
    }
}
