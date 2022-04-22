use std::collections::HashMap;

use crate::dasm::{Definition, Symbol, Word};
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Context {
    // symbols are used for address labels
    pub symbols: HashMap<Word, Vec<Symbol>>,

    // the address space
    pub address: Word,
    pub start_address: Word,
    pub end_address: Word,

    // flags may be used by parsers to store temporary values
    pub flags: HashMap<String, String>,

    // definitions are used for text replacement
    pub definitions: HashMap<Word, Definition>,
}

impl Context {
    pub fn new(address: Word, end_address: Word) -> Self {
        Self {
            symbols: HashMap::default(),
            address,
            start_address: address,
            end_address,
            flags: HashMap::default(),
            definitions: HashMap::default(),
        }
    }

    /// resets the current address back to start
    pub fn reset(&mut self) {
        self.flags.clear();
        self.address = self.start_address;
    }

    /// clears the entire context and resets it to its initial state
    pub fn clear(&mut self) {
        self.reset();
        self.symbols.clear();
        self.flags.clear();
        self.definitions.clear();
    }

    pub fn is_in_range(&self, addr: Word) -> bool {
        self.address <= addr && self.end_address > addr
    }

    pub fn is_at_end(&self) -> bool {
        !self.is_in_range(self.address)
    }

    /// adds a symbol the the symbol list
    pub fn add_symbol(&mut self, symbol: Symbol) {
        let address = symbol.address;
        if let Some(sl) = self.symbols.get_mut(&address) {
            sl.push(symbol);
            sl.sort_by(|l, r| l.order.cmp(&r.order));
        } else {
            self.add_first_symbol(symbol);
        }
    }

    /// adds a symbol if no other symbol for the given address exists
    pub fn add_first_symbol(&mut self, symbol: Symbol) {
        self.symbols
            .entry(symbol.address)
            .or_insert_with(|| vec![symbol]);
    }

    pub fn get_symbols(&self, addr: Word) -> Option<&Vec<Symbol>> {
        self.symbols.get(&addr)
    }

    pub fn advance(&mut self, by: Word) {
        self.address += by;
    }

    pub fn set_flag(&mut self, flag: &str, value: &str) {
        self.flags.insert(flag.into(), value.into());
    }

    pub fn unset_flag(&mut self, flag: &str) {
        self.flags.remove(flag);
    }

    pub fn get_flag(&self, flag: &str) -> Option<&str> {
        self.flags.get(flag).map(|x| &**x)
    }

    pub fn add_def(&mut self, def: Definition) {
        self.definitions.insert(def.value, def);
    }

    pub fn get_def(&mut self, value: Word) -> Option<&Definition> {
        self.definitions.get(&value)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test_context() -> Context {
        let ctx = Context::new(0x100, 0x110);

        ctx
    }

    #[test]
    fn it_should_be_in_address_range() {
        assert!(test_context().is_in_range(0x100), "lower bound");
        assert!(test_context().is_in_range(0x109), "middle");
        assert!(test_context().is_in_range(0x109), "upper bound");
    }

    #[test]
    fn it_should_be_outside_address_range() {
        assert!(!test_context().is_in_range(0x99), "less than");
        assert!(!test_context().is_in_range(0x110), "greater than");
    }

    #[test]
    fn it_should_be_at_end() {
        let mut ctx = test_context();
        assert!(!ctx.is_at_end(), "not at end");
        ctx.advance(0x10);
        assert!(ctx.is_at_end(), "at end");
    }

    #[test]
    fn it_should_add_symbols_and_definitions() {
        let mut ctx = test_context();
        ctx.add_symbol(Symbol::new(
            "",
            0x100,
            0,
            crate::dasm::SymbolAttributes::Std,
        ));
        ctx.add_def(Definition::new("test", 0x100, 1));

        assert_eq!(1, ctx.symbols.len());
        assert_eq!(1, ctx.definitions.len());
    }

    #[test]
    fn it_should_find_symbols() {
        let mut ctx = test_context();
        ctx.add_symbol(Symbol::new(
            "1",
            0x100,
            0,
            crate::dasm::SymbolAttributes::Std,
        ));
        ctx.add_symbol(Symbol::new(
            "2",
            0x100,
            0,
            crate::dasm::SymbolAttributes::Std,
        ));

        assert_eq!(
            Some(&vec![
                Symbol::new("1", 0x100, 0, crate::dasm::SymbolAttributes::Std),
                Symbol::new("2", 0x100, 0, crate::dasm::SymbolAttributes::Std),
            ]),
            ctx.get_symbols(0x100),
            "Get symbols"
        );

        assert_eq!(None, ctx.get_symbols(0x101), "Symbols do not exists");
    }

    #[test]
    fn it_should_find_definitions() {
        let mut ctx = test_context();
        ctx.add_def(Definition::new("test", 0x100, 1));

        assert_eq!(
            Some(&Definition::new("test", 0x100, 1)),
            ctx.get_def(0x100),
            "Definition should exist"
        );
        assert_eq!(None, ctx.get_def(0x99), "Definition should not exist");
    }

    #[test]
    fn it_should_manage_flags() {
        let mut ctx = test_context();
        ctx.set_flag("test", "123");

        assert_eq!(Some("123"), ctx.get_flag("test"), "Flag should be found");
        assert_eq!(None, ctx.get_flag("tes"), "Flag should not be found");

        ctx.unset_flag("test");
        assert_eq!(None, ctx.get_flag("test"), "Flag should have been removed");
    }
}
