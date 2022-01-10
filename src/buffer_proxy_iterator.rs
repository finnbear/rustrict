use std::collections::VecDeque;
use std::iter::Iterator;
use std::ops::RangeInclusive;

/// This iterator buffers characters until they can be determined to be clean of profanity.
pub(crate) struct BufferProxyIterator<I: Iterator<Item = char>> {
    iter: I,
    /// The index into iter of the start of buffer.
    buffer_start_position: usize,
    /// Staging area (to possibly censor).
    buffer: VecDeque<I::Item>,
}

impl<I: Iterator<Item = char>> BufferProxyIterator<I> {
    pub fn new(iter: I) -> Self {
        BufferProxyIterator {
            iter,
            buffer_start_position: 0,
            buffer: VecDeque::new(),
        }
    }

    /// Returns index of the last character read, or None if nothing has been read yet.
    pub fn index(&self) -> Option<usize> {
        if self.buffer_start_position + self.buffer.len() == 0 {
            // Didn't read anything yet.
            return None;
        }
        Some(self.buffer_start_position + self.buffer.len() - 1)
    }

    /// Returns index of the next character that can be spied, or empty if no characters can be spied.
    pub fn spy_next_index(&self) -> Option<usize> {
        if self.buffer.is_empty() {
            None
        } else {
            Some(self.buffer_start_position)
        }
    }

    /// Spies one one more character.
    pub fn spy_next(&mut self) -> Option<char> {
        let ret = self.buffer.pop_front();
        if ret.is_some() {
            self.buffer_start_position += 1;
        }
        ret
    }

    /// Censors a given range (must be fully resident in the buffer).
    pub fn censor(&mut self, range: RangeInclusive<usize>, replacement: char) {
        let start = self.buffer_start_position;
        for i in range {
            self.buffer[i - start] = replacement;
        }
    }
}

impl<I: Iterator<Item = char>> Iterator for BufferProxyIterator<I> {
    type Item = I::Item;

    fn next(&mut self) -> Option<Self::Item> {
        let ret = self.iter.next();
        if let Some(val) = ret.as_ref() {
            self.buffer.push_back(*val);
        }
        ret
    }
}
