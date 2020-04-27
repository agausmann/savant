use savant::position::Position;
use std::env;
use std::num::NonZeroUsize;
use std::process::exit;

fn usage() -> ! {
    eprintln!("Usage: perft <fen> <depth>");
    exit(1)
}

fn main() {
    let mut args = env::args();
    let fen = args.nth(1).unwrap_or_else(|| usage());
    let depth = args.next().unwrap_or_else(|| usage());

    let position: Position = fen.parse().expect("got invalid FEN");
    let depth: NonZeroUsize = depth.parse().expect("depth must be a positive integer");

    let mut total_nodes = 0;
    for move_ in position.dir_golem() {
        let mut new_position = position.clone();
        new_position.make_move(move_);
        let nodes = new_position.perft(depth.get() - 1);
        total_nodes += nodes;
        println!("{}: {}", move_, nodes);
    }
    println!();
    println!("Nodes searched: {}", total_nodes);
}
