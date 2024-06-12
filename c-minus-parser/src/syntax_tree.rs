use std::rc::Rc;

use id_tree::*;
use c_minus_token::Token;

#[derive(Debug, PartialEq)]
pub enum SyntaxType {
    Terminal(Rc<Token>),
    SyntaxTree,
    VariableDefine,
    Expr,
    Array,
    BooleanExpr,
    ExprOpt,
    StmtBlock,
    AssignStmt,
    IfStmt,
    ElseStmt,
    ReturnStmt,
    WhileLoop,
    FuncDeclare,
    FuncParam,
    FuncArg,
    FuncCall,
}

pub type SyntaxTree = Tree<SyntaxType>;

impl SyntaxType {
    pub fn token(&self) -> Option<Rc<Token>> {
        match *self {
            SyntaxType::Terminal(ref t) => Some(t.clone()),
            _ => None,
        }
    }

    pub fn symbol(&self) -> Option<&str> {
        match *self {
            SyntaxType::Terminal(ref tok) => {
                match &**tok {
                    &Token::Identifier(ref id, _) => Some(id),
                    _ => None,
                }
            },
            _ => None,
        }
    }
}