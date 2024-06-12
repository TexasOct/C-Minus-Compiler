use c_minus_lexer::lexer::MinusLexer;
use c_minus_parser::Parser;
use c_minus_parser::recursive_descent_parser::RecursiveDescentParser;

#[test]
fn test_example_1() {
    let src = "
int gcd (int u, int v) {
    if (v == 0)
        return u;
    else
        return gcd(v,u-u/v*v);
    // hello world
}
void main(void) {
    int x;
    int y;
    x = input();
    y = input();
    y = y + x;
    output(gcd(x,y));
}
";

    let mut parser = RecursiveDescentParser::new(MinusLexer::new(src.as_bytes()));
    parser.run().unwrap();
    parser.dump();
}

#[test]
fn test_example_2() {
    let src = "
int x[10];
int minloc ( int a[], int low, int high ) {
    int i; int x; int k;
    int j, k; // special
    k = low;
    x = a[low];
    i = low + 1;
    while (i < high) {
        if (a[i] < x) {
            x = a[i];
            k = i;
        }
        i = i + 1;
    }
    return k;
}
void sort ( int a[], int low, int high ) {
    int i; int k;
    i = low;
    while (i < high-1) {
        int t;
        k = minloc (a,i,high);
        t =a[k];
        a[k] = a[i];
        a[i] = t;
        i = i + 1;
    }
    return;
}
void main (void) {
    int i;
    i = 0;
    if (a = 5) {}
    while (i < 10) {
        x[i] = input;
        i = i + 1;
        sort (x,0,10);
        i = 0;
        while (i < 10) {
            output(x[i]);
            i = i + 1;
            i + 10;
        }
    }
}
";
    let mut parser = RecursiveDescentParser::new(MinusLexer::new(src.as_bytes()));
    parser.run().unwrap();
    parser.dump_lexer();
    parser.dump();
}
