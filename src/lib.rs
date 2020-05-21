mod lexer;

use lexer::Lexer;

pub fn parse(zonefile: &str) {
    let mut lexer = Lexer::new(zonefile);

    loop {
        let token = lexer.next_token();

        println!("Got me some {:?}", token);
    }
}
