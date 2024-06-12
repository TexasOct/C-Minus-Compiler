pub mod lexer;

use c_minus_token::Token;

type LexerResult = Result<Token, LexerError>;

#[derive(Debug)]
pub enum LexerError {
    Success,
    UnexpectEnd,
    UnexpectedChar(char, Vec<char>),
}

pub trait Lexer: Iterator<Item = Token> {}