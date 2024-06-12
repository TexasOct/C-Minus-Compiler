use std::fmt;
use std::fmt::Display;
use std::fmt::Formatter;

#[derive(Clone, Debug, PartialEq)]
pub enum KeyWords {
    If,
    Else,
    Int,
    Void,
    While,
    Return
}

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    NoType,
    SignedInt,
    Void,
}

impl KeyWords {
    pub fn is_type(&self) -> bool {
        match self {
            // Int | Void => true,
            &KeyWords::Int => true,
            &KeyWords::Void => true,
            _ => false,
        }
    }

    pub fn to_type(&self) -> Option<Type> {
        match *self {
            KeyWords::Int => Some(Type::SignedInt),
            _ => None,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Operators {
    Add,
    Assign,
    AddEqual,
    DoubleAdd,
    DoubleMinus,
    Division,
    Equal,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    LogicNot,
    Minus,
    MinusEqual,
    Mul,
    Not,
    NotEqual,
    Or,
    Xor,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Brackets {
    LeftParenthesis,
    RightParenthesis,
    LeftSquareBracket,
    RightSquareBracket,
    LeftCurlyBracket,
    RightCurlyBracket,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Numbers {
    SignedInt(isize),
}

impl Numbers {
    pub fn from_str<T: AsRef<str>>(s: T) -> Numbers {
        Numbers::SignedInt(s.as_ref().parse::<isize>().unwrap())
    }
}

#[derive(Debug, PartialEq)]
pub enum Token {
    Arrow,
    Asterisk,
    Bracket(Brackets),
    Comment(String),
    Comma,
    KeyWord(KeyWords),
    Number(Numbers),
    Operator(Operators),
    Preprocessor(String),
    Space,
    Semicolon,
    Identifier(String, Type),
}

pub fn is_keywords(s: &str) -> bool {
    Token::key_word_index(s).is_some()
}

impl Token {
    pub fn comment(c: &str) -> Token {
        Token::Comment(c.to_owned())
    }

    pub fn ident(v: &str) -> Token {
        Token::Identifier(v.to_owned(), Type::NoType)
    }

    pub fn key_word(k: &str) -> Token {
        const KEY_TOKEN: &'static [KeyWords] = &[
            KeyWords::If,
            KeyWords::Else,
            KeyWords::Int,
            KeyWords::Void,
            KeyWords::While,
            KeyWords::Return,
        ];
        let index = Token::key_word_index(k).unwrap();

        Token::KeyWord(KEY_TOKEN[index].clone())
    }

    fn key_word_index(s: &str) -> Option<usize> {
        const KEY_WORDS: &'static [&'static str] = &[
            "if",
            "else",
            "int",
            "void",
            "while",
            "return",
        ];

        KEY_WORDS.iter().position(|&x| x == s)
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            &Token::Arrow => write!(f, "arrow:\t\t '->'"),
            &Token::Space => write!(f, "space:\t\t ' '"),
            &Token::Semicolon => write!(f, "semicolon:\t ';'"),
            &Token::Asterisk => write!(f, "asterisk:\t '*'"),
            &Token::Comma => write!(f, "comma:\t\t ','"),
            &Token::Bracket(ref b) => write!(f, "bracket:\t {:?}", b),
            &Token::Number(ref n) => write!(f, "number:\t\t {:?}", n),
            &Token::Comment(ref s) => write!(f, "comment:\t {}", s),
            &Token::KeyWord(ref k) => write!(f, "keywords:\t {:?}", k),
            &Token::Operator(ref o) => write!(f, "operators:\t {:?}", o),
            &Token::Preprocessor(ref p) => write!(f, "preprocessor:\t {}", p),
            &Token::Identifier(ref v, ref t) => write!(f, "ident:\t {}({:?})", v, t),
        }
    }
}