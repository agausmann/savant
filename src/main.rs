use savant::uci::{Command, Uci};
use std::io;

fn main() -> io::Result<()> {
    let mut uci = Uci::new();

    loop {
        match uci.poll()? {
            Some(Command::Quit) => break,
            _ => {}
        }
    }
    Ok(())
}
