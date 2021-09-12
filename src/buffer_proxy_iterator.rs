use std::cell::RefCell;
use std::collections::VecDeque;
use std::iter::Iterator;
use std::ops::RangeInclusive;
use std::rc::Rc;

/// This iterator buffers characters until they can be determined to be clean of profanity.
pub(crate) struct BufferProxyIterator<I: Iterator<Item = char>> {
    inner: Rc<RefCell<SpyInner<I>>>,
}

struct SpyInner<I: Iterator<Item = char>> {
    iter: I,
    // The index into iter of the start of buffer.
    buffer_start_position: usize,
    buffer: VecDeque<I::Item>,
}

impl<I: Iterator<Item = char>> BufferProxyIterator<I> {
    pub fn new(iter: I) -> Self {
        BufferProxyIterator {
            inner: Rc::new(RefCell::new(SpyInner {
                iter,
                buffer_start_position: 0,
                buffer: VecDeque::new(),
            })),
        }
    }

    /// Returns index of the last character read, or None if nothing has been read yet.
    pub fn index(&self) -> Option<usize> {
        let inner = self.inner.borrow();
        if inner.buffer_start_position + inner.buffer.len() == 0 {
            // Didn't read anything yet.
            return None;
        }
        Some(inner.buffer_start_position + inner.buffer.len() - 1)
    }

    /// Returns index of the next character that can be spied, or empty if no characters can be spied.
    pub fn spy_next_index(&self) -> Option<usize> {
        let inner = self.inner.borrow();
        return if inner.buffer.is_empty() {
            None
        } else {
            Some(self.inner.borrow().buffer_start_position)
        };
    }

    /// Spies one one more character.
    pub fn spy_next(&self) -> Option<char> {
        let mut inner = self.inner.borrow_mut();
        let ret = inner.buffer.pop_front();
        if ret.is_some() {
            inner.buffer_start_position += 1;
        }
        ret
    }

    /// Censors a given range (must be fully resident in the buffer).
    pub fn censor(&self, range: RangeInclusive<usize>, replacement: char) {
        let mut inner = self.inner.borrow_mut();
        let start = inner.buffer_start_position;
        for i in range {
            inner.buffer[i - start] = replacement;
        }
    }
}

impl<I: Iterator<Item = char>> Clone for BufferProxyIterator<I> {
    fn clone(&self) -> Self {
        Self {
            inner: Rc::clone(&self.inner),
        }
    }
}

impl<I: Iterator<Item = char>> Iterator for BufferProxyIterator<I> {
    type Item = I::Item;

    fn next(&mut self) -> Option<Self::Item> {
        let mut inner = self.inner.borrow_mut();
        let ret = inner.iter.next();
        if let Some(val) = ret.as_ref() {
            inner.buffer.push_back(*val);
        }
        ret
    }
}
