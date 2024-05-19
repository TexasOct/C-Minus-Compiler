use c_minus_token::*;

use id_tree::*;

use std::rc::Rc;
use std::cell::RefCell;
use crate::*;
use crate::ParseError::{MultiDefineError, SemanticError};
use crate::symbol_manager::{ScopeGuard, SymbolManager};
use crate::syntax_tree::SyntaxType;
pub struct SymbolChecker<'t> {
    ast: &'t SyntaxTree,
    symbols: Rc<RefCell<SymbolManager<NodeId, String>>>,
}

macro_rules! error {
    ($err: ident) => {
        Err(ParseErrInfo{
            err_type: $err,
        })
    };
}

impl<'t> SymbolChecker<'t> {
    pub fn new(ast: &'t SyntaxTree) -> SymbolChecker<'t> {
        SymbolChecker {
            ast: ast,
            symbols: Rc::new(RefCell::new(SymbolManager::new())),
        }
    }

    pub fn check(&self) -> ParserResult {
        let ref root_id = self.ast.root_node_id().unwrap().clone();
        self.check_subtree(root_id)?;

        assert_eq!(self.symbols.borrow().scope_level(), 1);
        Ok(())
    }

    fn check_subtree(&self, root_id: &NodeId) -> ParserResult {
        for id in self.children_ids(root_id) {
            match self.ast.get(id).unwrap().data() {
                &SyntaxType::FuncDefine |
                &SyntaxType::FuncDeclare => self.check_func(id)?,
                &SyntaxType::VariableDefine => self.check_variable_define(id)?,
                t => println!("UNHANDLED check_subtree: {:?}", t),
            }
        }

        Ok(())
    }

    fn push_identifier(&self, id: &NodeId) -> ParserResult {
        match self.ast.get(id) {
            Ok(node) => {
                match node.data() {
                    SyntaxType::Array => {
                        return Ok(())
                    },
                    _ => {}
                }
            }
            Err(_) => {}
        }

        match *self.token(id).unwrap() {
            Token::Identifier(ref ident, _) => {
                if self.symbols.borrow_mut().push_symbol(ident, id.clone()).is_err() {
                    return error!(MultiDefineError);
                }
            },
            _ => unreachable!(),
        }

        Ok(())
    }

    // check a variable define stmt, if variable already defined, return error.
    fn check_variable_define(&self, root_id: &NodeId) -> ParserResult {
        for id in self.ast.children_ids(root_id).unwrap() {
            match self.ast.get(id) {
                Ok(node) => {
                    match node.data() {
                        SyntaxType::Array => {
                            self.check_variable_define(id)?;
                            continue
                        },
                        _ => {continue},
                    }
                }
                Err(_) => {}
            }

            match *self.token(id).unwrap() {
                Token::Identifier(_, _) => self.push_identifier(id)?,
                Token::KeyWord(_) => {},
                _ => return error!(SemanticError),
            }
        }

        Ok(())
    }

    fn check_func(&self, id: &NodeId) -> ParserResult {
        let ids = self.children_ids(&id);
        // check function name, function return type is index 0.
        self.push_identifier(ids[1])?;

        let _symbol_guard = self.scope_guard("");

        // check function arguments
        let mut index = 2;
        while index < ids.len() {
            match self.data(ids[index]) {
                &SyntaxType::FuncParam => self.check_func_arg(ids[index])?,
                _ => break,
            }

            index += 1;
        }

        Ok(())
    }

    fn check_func_arg(&self, id: &NodeId) -> ParserResult {
        let ids = self.children_ids(id);
        self.push_identifier(ids[1])?;

        Ok(())
    }

    #[inline]
    fn token(&self, node_id: &NodeId) -> Option<Rc<Token>> {
        self.data(node_id).token()
    }

    #[inline]
    fn data(&self, node_id: &NodeId) -> &SyntaxType {
        self.ast.get(node_id).unwrap().data()
    }

    #[inline]
    fn children_ids(&self, node_id: &NodeId) -> Vec<&NodeId> {
        self.ast.children_ids(&node_id).unwrap().collect()
    }

    #[inline]
    fn scope_guard<T: AsRef<str>>(&self, scope: T) -> ScopeGuard<NodeId, String> {
        ScopeGuard::new(self.symbols.clone(), scope.as_ref().to_owned())
    }
}