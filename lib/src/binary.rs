#[derive(Default, Builder, Debug, Clone)]
#[builder(setter(into))]
pub struct Binary {
    /// current offset
    current: usize,

    /// data buffer
    data: Vec<u8>,

    /// end offset
    end: usize,
}

impl Binary {
    /// reads data from current file starting at current offset
    pub fn read(&self, bytes: usize) -> &[u8] {
        todo!()
    }

    /// advances the current pointer by n bytes
    pub fn advance(&mut self, bytes: usize) {
        self.current += bytes;
    }
}
