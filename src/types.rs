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

    pub fn back_rank(self) -> Rank {
        match self {
            Color::White => Rank::R1,
            Color::Black => Rank::R8,
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
