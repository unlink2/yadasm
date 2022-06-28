use std::collections::BTreeMap;

pub type Address = u64;

/// Recursive env for the parser to use as a label lookup
pub struct Env {
    defs: BTreeMap<Address, Def>,
    labels: BTreeMap<Address, Def>,

    // parent env
    parent: Option<Box<Env>>,
}

/// A general purpose definition
pub struct Def {
    name: String,
}
