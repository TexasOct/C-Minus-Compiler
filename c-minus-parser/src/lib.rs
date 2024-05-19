pub mod symbol_parser;

pub mod syntax_tree;
mod parser;

#[derive(Debug)]
pub enum ParseError {
    SyntaxError,
    SemanticError,
    MultiDefineError,
    UndefinedSymbol,
}

#[derive(Debug)]
pub struct ParseErrInfo {
    err_type: ParseError,
}

type ParserResult = Result<(), ParseErrInfo>;