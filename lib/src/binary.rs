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
}
