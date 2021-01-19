//! UCI interface.

use crate::types::Move;
use std::io::{self, BufRead, Write};
use std::sync::mpsc;
use std::thread;
use std::time::Duration;

pub struct Uci {
    _stdin_thread: thread::JoinHandle<()>,
    line_rx: mpsc::Receiver<io::Result<String>>,
    writer: Box<dyn Write>,
}

impl Uci {
    pub fn new() -> Self {
        let (line_tx, line_rx) = mpsc::channel();
        let _stdin_thread = thread::spawn(move || {
            let stdin = io::stdin();
            let reader = stdin.lock();

            for maybe_line in reader.lines() {
                match line_tx.send(maybe_line) {
                    Ok(_) => {}
                    Err(_) => break,
                }
            }
        });
        Self {
            _stdin_thread,
            line_rx,
            writer: Box::new(io::stdout()) as _,
        }
    }

    pub fn poll(&mut self) -> io::Result<Option<Command>> {
        loop {
            let line = match self.line_rx.try_recv() {
                Ok(maybe_line) => maybe_line?,
                Err(mpsc::TryRecvError::Empty) => return Ok(None),
                Err(mpsc::TryRecvError::Disconnected) => return Ok(Some(Command::Quit)),
            };
            let mut words = line.split_whitespace().peekable();
            let command = match words.next() {
                Some(word) => word,
                None => continue,
            };

            match command {
                "uci" => {
                    writeln!(
                        self.writer,
                        "id name {} {}",
                        env!("CARGO_PKG_NAME"),
                        env!("CARGO_PKG_VERSION")
                    )?;
                    writeln!(self.writer, "id author {}", env!("CARGO_PKG_AUTHORS"))?;
                    writeln!(self.writer, "uciok")?;
                }
                "isready" => {
                    writeln!(self.writer, "readyok")?;
                }
                "debug" => {
                    let enabled = match words.next() {
                        Some("on") => true,
                        Some("off") => false,
                        _ => continue,
                    };
                    return Ok(Some(Command::Debug { enabled }));
                }
                "setoption" => {
                    let mut name = None;
                    let mut value = None;
                    while let Some(word) = words.next() {
                        match word {
                            "name" => {
                                if let Some(word) = words.next() {
                                    name = Some(word.to_string());
                                }
                            }
                            "value" => {
                                if let Some(word) = words.next() {
                                    value = Some(word.to_string());
                                }
                            }
                            _ => {}
                        }
                    }
                    let name = match name {
                        Some(s) => s,
                        None => continue,
                    };
                    return Ok(Some(Command::SetOption { name, value }));
                }
                "ucinewgame" => {
                    return Ok(Some(Command::NewGame));
                }
                "position" => {
                    let mut position = None;
                    let mut moves = Vec::new();
                    while let Some(word) = words.next() {
                        match word {
                            "fen" => {
                                if let Some(word) = words.next() {
                                    position = Some(Position::Fen(word.to_string()));
                                }
                            }
                            "startpos" => {
                                position = Some(Position::Start);
                            }
                            "moves" => {
                                moves.clear();
                                while let Some(move_) = words.peek().and_then(|s| s.parse().ok()) {
                                    moves.push(move_);
                                    words.next();
                                }
                            }
                            _ => {}
                        }
                    }
                    return Ok(Some(Command::Position { position, moves }));
                }
                "go" => {
                    let mut white_time = None;
                    let mut black_time = None;
                    let mut white_increment = None;
                    let mut black_increment = None;
                    let mut moves_to_go = None;
                    let mut max_depth = None;
                    let mut max_nodes = None;
                    let mut mate_depth = None;
                    let mut max_time = None;
                    let mut infinite = false;
                    let mut ponder = false;
                    let mut search_moves = None;

                    while let Some(word) = words.next() {
                        match word {
                            "searchmoves" => {
                                let mut moves = Vec::new();
                                while let Some(move_) = words.peek().and_then(|s| s.parse().ok()) {
                                    moves.push(move_);
                                    words.next();
                                }
                                search_moves = Some(moves);
                            }
                            "ponder" => {
                                ponder = true;
                            }
                            "wtime" => {
                                if let Some(millis) =
                                    words.next().and_then(|word| word.parse().ok())
                                {
                                    white_time = Some(Duration::from_millis(millis));
                                }
                            }
                            "btime" => {
                                if let Some(millis) =
                                    words.next().and_then(|word| word.parse().ok())
                                {
                                    black_time = Some(Duration::from_millis(millis));
                                }
                            }
                            "winc" => {
                                if let Some(millis) =
                                    words.next().and_then(|word| word.parse().ok())
                                {
                                    white_increment = Some(Duration::from_millis(millis));
                                }
                            }
                            "binc" => {
                                if let Some(millis) =
                                    words.next().and_then(|word| word.parse().ok())
                                {
                                    black_increment = Some(Duration::from_millis(millis));
                                }
                            }
                            "movestogo" => {
                                if let Some(num) = words.next().and_then(|word| word.parse().ok()) {
                                    moves_to_go = Some(num);
                                }
                            }
                            "depth" => {
                                if let Some(num) = words.next().and_then(|word| word.parse().ok()) {
                                    max_depth = Some(num);
                                }
                            }
                            "nodes" => {
                                if let Some(num) = words.next().and_then(|word| word.parse().ok()) {
                                    max_nodes = Some(num);
                                }
                            }
                            "mate" => {
                                if let Some(num) = words.next().and_then(|word| word.parse().ok()) {
                                    mate_depth = Some(num);
                                }
                            }
                            "movetime" => {
                                if let Some(millis) =
                                    words.next().and_then(|word| word.parse().ok())
                                {
                                    max_time = Some(Duration::from_millis(millis));
                                }
                            }
                            "infinite" => {
                                infinite = true;
                            }
                            _ => {}
                        }
                    }

                    return Ok(Some(Command::Go {
                        white_time,
                        black_time,
                        white_increment,
                        black_increment,
                        moves_to_go,
                        max_depth,
                        max_nodes,
                        mate_depth,
                        max_time,
                        infinite,
                        ponder,
                        search_moves,
                    }));
                }
                "stop" => {
                    return Ok(Some(Command::Stop));
                }
                "ponderhit" => {
                    return Ok(Some(Command::PonderHit));
                }
                "quit" => {
                    return Ok(Some(Command::Quit));
                }
                _ => {}
            }
        }
    }
}

pub enum Command {
    Debug {
        enabled: bool,
    },
    SetOption {
        name: String,
        value: Option<String>,
    },
    NewGame,
    Position {
        position: Option<Position>,
        moves: Vec<Move>,
    },
    Go {
        white_time: Option<Duration>,
        black_time: Option<Duration>,
        white_increment: Option<Duration>,
        black_increment: Option<Duration>,
        moves_to_go: Option<u32>,
        max_depth: Option<u32>,
        max_nodes: Option<u32>,
        mate_depth: Option<u32>,
        max_time: Option<Duration>,
        infinite: bool,
        ponder: bool,
        search_moves: Option<Vec<Move>>,
    },
    Stop,
    PonderHit,
    Quit,
}

pub enum Position {
    Start,
    Fen(String),
}
