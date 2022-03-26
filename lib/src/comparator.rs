pub type Comparator<T> = dyn Fn(T) -> bool;

pub fn always_true<T>(_n: T) -> bool {
    true
}

pub fn always_false<T>(_n: T) -> bool {
    false
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_should_always_be_true() {
        assert!(always_true(1));
    }

    #[test]
    fn it_should_always_be_false() {
        assert!(!always_false(1));
    }
}
