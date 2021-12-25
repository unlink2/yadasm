pub enum Endianness {
    Big,
    Little,
}

impl Default for Endianness {
    fn default() -> Self {
        // contorversial default choice
        Self::Little
    }
}

pub struct Node {
    endianess: Endianness,
}
