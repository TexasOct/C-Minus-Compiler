extern crate core;

use c_minus_lexer::lexer::MinusLexer;
use c_minus_parser::Parser;
use c_minus_parser::recursive_descent_parser::RecursiveDescentParser;

fn main() {
    let src = "
int gcd(int u, int v)
{
    if (v == 0)
        return u ;
    else
        return gcd(v,u-u/v*v);
/* u-u/v*v == u mod v */
}
void main(void)
{
    int x;
    int y;
    x = input();
    y = input();
    output(gcd(x,y));
}";
    let mut parser = RecursiveDescentParser::new(MinusLexer::new(src.as_bytes()));

    println!("\n{}\n", src);

    println!("result: {:?}\n", parser.run());
    parser.dump();
}