use std::env::args;

fn main() {
    let args: Vec<String> = args().collect();
    if args.len() < 2 {
        println!("Usage: {} <name>", args[0]);
        return;
    }
    let src_name = &args[1];

    println!("Hello, {}!", src_name);
}
