use crate::bitboard::Bitboard;
use enum_map::Enum;
use std::fmt::{self, Write};
use std::str::FromStr;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Enum)]
pub enum Piece {
    Pawn,
    Knight,
    Bishop,
    Rook,
    Queen,
    King,
}

impl Piece {
    pub fn to_char(self) -> char {
        match self {
            Piece::Pawn => 'P',
            Piece::Knight => 'N',
            Piece::Bishop => 'B',
            Piece::Rook => 'R',
            Piece::Queen => 'Q',
            Piece::King => 'K',
        }
    }

    pub fn from_char(c: char) -> Option<Piece> {
        match c.to_ascii_uppercase() {
            'P' => Some(Piece::Pawn),
            'N' => Some(Piece::Knight),
            'B' => Some(Piece::Bishop),
            'R' => Some(Piece::Rook),
            'Q' => Some(Piece::Queen),
            'K' => Some(Piece::King),
            _ => None,
        }
    }
}

impl fmt::Display for Piece {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_char(self.to_char())
    }
}

impl FromStr for Piece {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        s.parse().map_err(|e| format!("{}", e)).and_then(|c| {
            Self::from_char(c).ok_or(format!("`{}` does not correspond to a piece type", c))
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Enum)]
pub enum Color {
    White,
    Black,
}

impl Color {
    pub fn to_char(self) -> char {
        match self {
            Color::White => 'w',
            Color::Black => 'b',
        }
    }

    pub fn from_char(c: char) -> Option<Color> {
        match c.to_ascii_lowercase() {
            'w' => Some(Color::White),
            'b' => Some(Color::Black),
            _ => None,
        }
    }

    pub fn enemy(self) -> Color {
        match self {
            Color::White => Color::Black,
            Color::Black => Color::White,
        }
    }

    pub fn back_rank(self) -> Rank {
        match self {
            Color::White => Rank::R1,
            Color::Black => Rank::R8,
        }
    }

    pub fn double_push_rank(self) -> Rank {
        match self {
            Color::White => Rank::R4,
            Color::Black => Rank::R5,
        }
    }
}

impl fmt::Display for Color {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_char(self.to_char())
    }
}

impl FromStr for Color {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        s.parse().map_err(|e| format!("{}", e)).and_then(|c| {
            Self::from_char(c).ok_or(format!("`{}` does not correspond to a color", c))
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ColoredPiece(pub Piece, pub Color);

impl ColoredPiece {
    pub fn piece(self) -> Piece {
        self.0
    }

    pub fn color(self) -> Color {
        self.1
    }

    pub fn to_char(self) -> char {
        match self.color() {
            Color::White => self.piece().to_char().to_ascii_uppercase(),
            Color::Black => self.piece().to_char().to_ascii_lowercase(),
        }
    }

    pub fn from_char(c: char) -> Option<ColoredPiece> {
        let piece = Piece::from_char(c)?;
        let color = if c.is_ascii_uppercase() {
            Color::White
        } else {
            Color::Black
        };
        Some(ColoredPiece(piece, color))
    }
}

impl fmt::Display for ColoredPiece {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_char(self.to_char())
    }
}

impl FromStr for ColoredPiece {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        s.parse().map_err(|e| format!("{}", e)).and_then(|c| {
            Self::from_char(c).ok_or(format!("`{}` does not correspond to a piece type", c))
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Enum)]
pub enum Rank {
    R1,
    R2,
    R3,
    R4,
    R5,
    R6,
    R7,
    R8,
}

impl Rank {
    pub(crate) fn bitboard_offset(self) -> u8 {
        match self {
            Rank::R1 => 0,
            Rank::R2 => 8,
            Rank::R3 => 16,
            Rank::R4 => 24,
            Rank::R5 => 32,
            Rank::R6 => 40,
            Rank::R7 => 48,
            Rank::R8 => 56,
        }
    }

    pub fn to_char(self) -> char {
        match self {
            Rank::R1 => '1',
            Rank::R2 => '2',
            Rank::R3 => '3',
            Rank::R4 => '4',
            Rank::R5 => '5',
            Rank::R6 => '6',
            Rank::R7 => '7',
            Rank::R8 => '8',
        }
    }

    pub fn from_char(c: char) -> Option<Rank> {
        match c {
            '1' => Some(Rank::R1),
            '2' => Some(Rank::R2),
            '3' => Some(Rank::R3),
            '4' => Some(Rank::R4),
            '5' => Some(Rank::R5),
            '6' => Some(Rank::R6),
            '7' => Some(Rank::R7),
            '8' => Some(Rank::R8),
            _ => None,
        }
    }

    pub fn north(self) -> Option<Rank> {
        match self {
            Rank::R1 => Some(Rank::R2),
            Rank::R2 => Some(Rank::R3),
            Rank::R3 => Some(Rank::R4),
            Rank::R4 => Some(Rank::R5),
            Rank::R5 => Some(Rank::R6),
            Rank::R6 => Some(Rank::R7),
            Rank::R7 => Some(Rank::R8),
            Rank::R8 => None,
        }
    }

    pub fn south(self) -> Option<Rank> {
        match self {
            Rank::R1 => None,
            Rank::R2 => Some(Rank::R1),
            Rank::R3 => Some(Rank::R2),
            Rank::R4 => Some(Rank::R3),
            Rank::R5 => Some(Rank::R4),
            Rank::R6 => Some(Rank::R5),
            Rank::R7 => Some(Rank::R6),
            Rank::R8 => Some(Rank::R7),
        }
    }
}

impl fmt::Display for Rank {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_char(self.to_char())
    }
}

impl FromStr for Rank {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        s.parse().map_err(|e| format!("{}", e)).and_then(|c| {
            Self::from_char(c).ok_or(format!("`{}` does not correspond to a rank", c))
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Enum)]
pub enum File {
    Fa,
    Fb,
    Fc,
    Fd,
    Fe,
    Ff,
    Fg,
    Fh,
}

impl File {
    pub(crate) fn bitboard_offset(self) -> u8 {
        match self {
            File::Fa => 0,
            File::Fb => 1,
            File::Fc => 2,
            File::Fd => 3,
            File::Fe => 4,
            File::Ff => 5,
            File::Fg => 6,
            File::Fh => 7,
        }
    }

    pub fn to_char(self) -> char {
        match self {
            File::Fa => 'a',
            File::Fb => 'b',
            File::Fc => 'c',
            File::Fd => 'd',
            File::Fe => 'e',
            File::Ff => 'f',
            File::Fg => 'g',
            File::Fh => 'h',
        }
    }

    pub fn from_char(c: char) -> Option<File> {
        match c.to_ascii_lowercase() {
            'a' => Some(File::Fa),
            'b' => Some(File::Fb),
            'c' => Some(File::Fc),
            'd' => Some(File::Fd),
            'e' => Some(File::Fe),
            'f' => Some(File::Ff),
            'g' => Some(File::Fg),
            'h' => Some(File::Fh),
            _ => None,
        }
    }

    pub fn east(self) -> Option<File> {
        match self {
            File::Fa => Some(File::Fb),
            File::Fb => Some(File::Fc),
            File::Fc => Some(File::Fd),
            File::Fd => Some(File::Fe),
            File::Fe => Some(File::Ff),
            File::Ff => Some(File::Fg),
            File::Fg => Some(File::Fh),
            File::Fh => None,
        }
    }

    pub fn west(self) -> Option<File> {
        match self {
            File::Fa => None,
            File::Fb => Some(File::Fa),
            File::Fc => Some(File::Fb),
            File::Fd => Some(File::Fc),
            File::Fe => Some(File::Fd),
            File::Ff => Some(File::Fe),
            File::Fg => Some(File::Ff),
            File::Fh => Some(File::Fg),
        }
    }
}

impl fmt::Display for File {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_char(self.to_char())
    }
}

impl FromStr for File {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        s.parse().map_err(|e| format!("{}", e)).and_then(|c| {
            Self::from_char(c).ok_or_else(|| format!("`{}` does not correspond to a file", c))
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RankFile(pub Rank, pub File);

impl RankFile {
    pub fn rank(self) -> Rank {
        self.0
    }

    pub fn file(self) -> File {
        self.1
    }

    pub(crate) fn bitboard_offset(self) -> u8 {
        self.rank().bitboard_offset() + self.file().bitboard_offset()
    }
}

impl fmt::Display for RankFile {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_char(self.file().to_char())?;
        f.write_char(self.rank().to_char())?;
        Ok(())
    }
}

impl FromStr for RankFile {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let boundary = s
            .char_indices()
            .nth(1)
            .ok_or_else(|| format!("not enough characters"))?
            .0;
        let file = s[..boundary].parse()?;
        let rank = s[boundary..].parse()?;
        Ok(RankFile(rank, file))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Enum)]
pub enum Direction {
    North,
    South,
    East,
    West,
    NorthEast,
    NorthWest,
    SouthEast,
    SouthWest,
}

impl Direction {
    pub fn opposite(self) -> Direction {
        match self {
            Direction::North => Direction::South,
            Direction::South => Direction::North,
            Direction::East => Direction::West,
            Direction::West => Direction::East,
            Direction::NorthEast => Direction::SouthWest,
            Direction::NorthWest => Direction::SouthEast,
            Direction::SouthEast => Direction::NorthWest,
            Direction::SouthWest => Direction::NorthEast,
        }
    }

    pub fn forward(color: Color) -> Direction {
        match color {
            Color::White => Direction::North,
            Color::Black => Direction::South,
        }
    }

    pub fn forward_east(color: Color) -> Direction {
        match color {
            Color::White => Direction::NorthEast,
            Color::Black => Direction::SouthEast,
        }
    }

    pub fn forward_west(color: Color) -> Direction {
        match color {
            Color::White => Direction::NorthWest,
            Color::Black => Direction::SouthWest,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Enum)]
pub enum KnightDirection {
    NorthNorthEast,
    NorthEastEast,
    NorthNorthWest,
    NorthWestWest,
    SouthSouthEast,
    SouthEastEast,
    SouthSouthWest,
    SouthWestWest,
}

impl KnightDirection {
    pub fn opposite(self) -> KnightDirection {
        match self {
            KnightDirection::NorthNorthEast => KnightDirection::SouthSouthWest,
            KnightDirection::NorthEastEast => KnightDirection::SouthWestWest,
            KnightDirection::NorthNorthWest => KnightDirection::SouthSouthEast,
            KnightDirection::NorthWestWest => KnightDirection::SouthEastEast,
            KnightDirection::SouthSouthEast => KnightDirection::NorthNorthWest,
            KnightDirection::SouthEastEast => KnightDirection::NorthWestWest,
            KnightDirection::SouthSouthWest => KnightDirection::NorthNorthEast,
            KnightDirection::SouthWestWest => KnightDirection::NorthEastEast,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Move {
    pub source: RankFile,
    pub target: RankFile,
}

impl fmt::Display for Move {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}{}", self.source, self.target)
    }
}

impl FromStr for Move {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let boundary = s
            .char_indices()
            .nth(2)
            .ok_or_else(|| format!("not enough characters"))?
            .0;
        let source = s[..boundary].parse()?;
        let target = s[boundary..].parse()?;
        Ok(Move { source, target })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BitMove {
    pub source: Bitboard,
    pub target: Bitboard,
}

impl fmt::Display for BitMove {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", Move::from(*self))
    }
}

impl FromStr for BitMove {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        s.parse::<Move>().map(Into::into)
    }
}

impl From<Move> for BitMove {
    fn from(move_: Move) -> BitMove {
        BitMove {
            source: Bitboard::square(move_.source),
            target: Bitboard::square(move_.target),
        }
    }
}

impl From<BitMove> for Move {
    fn from(move_: BitMove) -> Move {
        Move {
            source: move_.source.ls1b().expect("empty source in bitmove"),
            target: move_.target.ls1b().expect("empty target in bitmove"),
        }
    }
}
