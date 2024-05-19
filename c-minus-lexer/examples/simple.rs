use c_minus_lexer::lexer;

fn main() {
    let s = "int main() {
    int a = 4;
    while (a > 0) {
        a = a - 1;
    }
    return a;
}";
    let mut lexer = lexer::MinusLexer::new(s.as_bytes());

    println!("{}", s);
    while let Some(tok) = lexer.next() {
        println!("{:?}", tok);
    }
}
