use std::env;

use yadasm::exec_cli;

fn main() {
    exec_cli(&env::args().collect::<Vec<String>>())
}
