pub type ReadOp = fn(usize, &[u8]) -> Option<Word>;
pub type Word = usize;

pub fn readnle(n: usize, bytes: &[u8]) -> Option<Word> {
    if n <= bytes.len() {
        Some(
            bytes[0..n]
                .iter()
                .enumerate()
                .rfold(0, |prev, (_, b)| prev * 256 + *b as Word),
        )
    } else {
        None
    }
}

pub fn readnbe(n: usize, bytes: &[u8]) -> Option<Word> {
    if n <= bytes.len() {
        Some(
            bytes[0..n]
                .iter()
                .enumerate()
                .fold(0, |prev, (_, b)| prev * 256 + *b as Word),
        )
    } else {
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_should_read_bytes_le() {
        assert_eq!(
            Some(0xddccbbaa),
            readnle(4, &[0xaa, 0xbb, 0xcc, 0xdd]),
            "Should read little endian"
        );

        assert_eq!(
            Some(0xddccbbaa),
            readnle(4, &[0xaa, 0xbb, 0xcc, 0xdd, 0xee]),
            "Should read little endian"
        );

        assert_eq!(
            None,
            readnle(4, &[0xaa, 0xbb, 0xcc]),
            "Should fail if not enough data"
        );
    }

    #[test]
    fn it_should_read_bytes_be() {
        assert_eq!(
            Some(0xaabbccdd),
            readnbe(4, &[0xaa, 0xbb, 0xcc, 0xdd]),
            "Should read big endian"
        );

        assert_eq!(
            Some(0xaabbccdd),
            readnbe(4, &[0xaa, 0xbb, 0xcc, 0xdd, 0xee]),
            "Should read big endian"
        );

        assert_eq!(
            None,
            readnbe(4, &[0xaa, 0xbb, 0xcc]),
            "Should fail if not enough data"
        );
    }
}
