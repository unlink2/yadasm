#![cfg_attr(feature = "no_std", no_std)]
#![cfg_attr(feature = "no_std", feature(alloc))]
pub mod dasm;

#[cfg(feature = "interface")]
mod interface;
pub mod prelude;

#[cfg(feature = "interface")]
pub use interface::*;
