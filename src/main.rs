#![feature(ascii_char)]

use std::fs::read;
use std::process::exit;
use clap::{
    arg,
    ArgMatches,
    command
};
use c_minus_lexer::lexer::MinusLexer;
use c_minus_parser::{
    Parser,
    recursive_descent_parser::RecursiveDescentParser
};

fn command() -> ArgMatches {
    command!()
        .arg(arg!(--file <VALUE>).short('f').required(true))
        .arg(arg!(--dump_lexer).short('l'))
        .arg(arg!(--dump_parser).short('p'))
        .get_matches()
}

fn execute_parser(args: ArgMatches) {
    let file_path = args.get_one::<String>("file").unwrap();

    let file= match read(file_path) {
        Ok(slice) =>
            slice.iter().fold(
                String::new(),
             |mut str, &byte| {
                    str.push(byte.as_ascii().unwrap().to_char());
                    str
                }
            ),
        Err(_) => exit(-1),
    };

    println!("{:}", file);
    let file = file.as_str();
    let mut parser = RecursiveDescentParser::new(MinusLexer::new(file.as_bytes()));
    parser.run().unwrap();
    if args.contains_id("dump_lexer") {
        parser.dump_lexer();
    }
    if args.contains_id("dump_parser") {
        parser.dump();
    }
}

fn main() {
    let args = command();
    execute_parser(args)
}
