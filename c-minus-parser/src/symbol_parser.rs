use std::rc::Rc;

use c_minus_token::*;
use crate::{ParseErrInfo, ParseError};

pub struct SymbolParser {
    tokens: Vec<Rc<Token>>

}

impl SymbolParser {
    pub fn new(tokens: Vec<Rc<Token>>) -> Self {
        SymbolParser {
            tokens
        }
    }

    pub fn parser(&mut self) {

    }

    fn symbol() {

    }
}