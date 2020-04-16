use enum_map::Enum;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Enum)]
pub enum Piece {
    Pawn,
    Knight,
    Bishop,
    Rook,
    Queen,
    King,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Enum)]
pub enum Color {
    White,
    Black,
}

impl Color {
    pub fn enemy(self) -> Color {
        match self {
            Color::White => Color::Black,
            Color::Black => Color::White,
        }
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RankFile(pub Rank, pub File);

impl RankFile {
    pub fn rank(self) -> Rank {
        self.0
    }

    pub fn file(self) -> File {
        self.1
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
