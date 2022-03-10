use rmonkey::repl;
fn main() {
    println!("Hello! This is the Monkey programming language");
    repl::start(std::io::stdin(), std::io::stdout()).unwrap();
}
