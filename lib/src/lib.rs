#![cfg_attr(feature = "no_std", no_std)]
#![cfg_attr(feature = "no_std", feature(alloc))]
pub mod dasm;

pub mod error;
pub mod prelude;

#[cfg(feature = "interface")]
pub mod interface;
#[cfg(feature = "interface")]
pub use interface::*;
