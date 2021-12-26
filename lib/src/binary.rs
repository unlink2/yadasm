pub trait Binary {
    fn read(&self, bytes: usize) -> Option<&[u8]>;
    fn advance(&mut self, bytes: usize);

    fn is_at_end(&self) -> bool;

    fn offset(&self) -> usize;
    fn len(&self) -> usize;
    fn is_empty(&self) -> bool;
}

#[derive(Default, Builder, Debug, Clone)]
#[builder(setter(into))]
pub struct BinaryFile {
    /// current offset
    #[builder(default)]
    current: usize,

    /// data buffer
    #[builder(default)]
    data: Vec<u8>,

    /// end offset
    #[builder(default)]
    end: Option<usize>,
}

impl Binary for BinaryFile {
    /// reads data from current file starting at current offset
    fn read(&self, bytes: usize) -> Option<&[u8]> {
        if self.len() >= self.offset() + bytes {
            Some(&self.data[self.offset()..self.offset() + bytes])
        } else {
            None
        }
    }

    /// advances the current pointer by n bytes
    fn advance(&mut self, bytes: usize) {
        self.current += bytes;
    }

    fn is_at_end(&self) -> bool {
        self.offset() >= self.len()
    }

    fn offset(&self) -> usize {
        self.current
    }

    fn len(&self) -> usize {
        usize::min(self.data.len(), self.end.unwrap_or(usize::MAX))
    }

    fn is_empty(&self) -> bool {
        self.data.is_empty()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_should_read() {
        let mut data = BinaryFileBuilder::default()
            .data(vec![1, 2, 3, 4, 5])
            .build()
            .unwrap();

        assert_eq!(data.read(2), Some(&[1_u8, 2_u8][..]));
        assert_eq!(data.offset(), 0);
        assert!(!data.is_at_end());
        data.advance(2);

        assert_eq!(data.read(2), Some(&[3_u8, 4_u8][..]));
        assert_eq!(data.offset(), 2);
        assert!(!data.is_at_end());
        data.advance(2);

        assert_eq!(data.read(2), None);
        assert_eq!(data.offset(), 4);
        assert!(!data.is_at_end());

        assert_eq!(data.read(1), Some(&[5_u8][..]));
        data.advance(1);
        assert_eq!(data.offset(), 5);
        assert!(data.is_at_end());

        assert_eq!(data.read(1), None);
    }
}
