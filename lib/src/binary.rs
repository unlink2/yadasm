pub trait Binary {
    fn read(&self, bytes: usize) -> Option<&[u8]>;
    fn advance(&mut self, bytes: usize);
}

#[derive(Default, Builder, Debug, Clone)]
#[builder(setter(into))]
pub struct BinaryFile {
    /// current offset
    current: usize,

    /// data buffer
    data: Vec<u8>,

    /// end offset
    end: usize,
}

impl Binary for BinaryFile {
    /// reads data from current file starting at current offset
    fn read(&self, bytes: usize) -> Option<&[u8]> {
        todo!()
    }

    /// advances the current pointer by n bytes
    fn advance(&mut self, bytes: usize) {
        self.current += bytes;
    }
}
