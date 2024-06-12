pub mod syntax_tree;
pub mod recursive_descent_parser;

use id_tree::NodeId;
use crate::syntax_tree::SyntaxTree;

#[derive(Debug)]
pub enum ParseError {
    SyntaxError(String),
    SemanticError,
    MultiDefineError,
    UndefinedSymbol,
}

#[derive(Debug)]
pub struct ParseErrInfo {
    #[allow(dead_code)]
    err_type: ParseError,
}

type ParserResult = Result<(), ParseErrInfo>;

pub trait Parser {
    fn run(&mut self) -> ParserResult;
    fn syntax_tree(&self) -> &SyntaxTree;
}

fn print_space(indentation: usize) {
    for i in 0..indentation {
        match i % 4 {
            0 => print!("|  "),
            1 => print!(":  "),
            2 => print!("!  "),
            3 => print!(".  "),
            _ => {},
        }
    }
}

fn dump_tree(tree: &SyntaxTree, root: &NodeId, indentation: usize) {

    // print root
    print_space(indentation);
    println!("{:?}", tree.get(root).unwrap().data());

    for node in tree.children(root).unwrap() {
        print_space(indentation + 1);
        println!("{:?}", node.data());

        for child in node.children() {
            dump_tree(tree, child, indentation + 2);
        }
    }
}
