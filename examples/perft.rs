use savant::position::Position;
use std::env;
use std::num::NonZeroUsize;
use std::process::exit;

fn usage() -> ! {
    eprintln!("Usage: perft <depth> <fen> [<move> ...]");
    exit(1)
}

fn main() {
    let mut args = env::args();
    let depth = args.nth(1).unwrap_or_else(|| usage());
    let fen = args.next().unwrap_or_else(|| usage());
    let moves = args
        .flat_map(|s| {
            s.split_whitespace()
                .map(ToString::to_string)
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();

    let depth: NonZeroUsize = depth.parse().expect("depth must be a positive integer");
    let mut position: Position = fen.parse().expect("got invalid FEN");
    for move_ in moves {
        position.make_move(move_.parse().unwrap());
    }

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
