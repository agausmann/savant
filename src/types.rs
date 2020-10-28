//! Types defining simple chess concepts.

use crate::bitboard::Bitboard;
use enum_map::Enum;
use std::fmt::{self, Write};
use std::str::FromStr;

/// Piece types.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Enum)]
pub enum Piece {
    /// Pawn `P`
    Pawn,

    /// Knight `N`
    Knight,

    /// Bishop `B`
    Bishop,

    /// Rook `R`
    Rook,

    /// Queen `Q`
    Queen,

    /// King `K`
    King,
}

impl Piece {
    /// Returns the character corresponding to the piece type.
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

    /// Parses a piece type from the given character, if it corresponds to a piece.
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

/// Colors / Players.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Enum)]
pub enum Color {
    /// "White" player `w`, first to move.
    White,

    /// "Black" player `b`, second to move.
    Black,
}

impl Color {
    /// Returns a character corresponding to the given color.
    pub fn to_char(self) -> char {
        match self {
            Color::White => 'w',
            Color::Black => 'b',
        }
    }

    /// Parses a color from the given character, if it corresponds to a color.
    pub fn from_char(c: char) -> Option<Color> {
        match c.to_ascii_lowercase() {
            'w' => Some(Color::White),
            'b' => Some(Color::Black),
            _ => None,
        }
    }

    /// The opposite of the given color.
    pub fn enemy(self) -> Color {
        match self {
            Color::White => Color::Black,
            Color::Black => Color::White,
        }
    }

    /// The rank on which the color's pieces (except for pawns) start.
    pub fn back_rank(self) -> Rank {
        match self {
            Color::White => Rank::R1,
            Color::Black => Rank::R8,
        }
    }

    /// The rank to which its pawns can be double-pushed.
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

/// Combination of `Piece` and `Color`, identifying a piece and the player who owns it.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ColoredPiece(pub Piece, pub Color);

impl ColoredPiece {
    /// The piece type.
    pub fn piece(self) -> Piece {
        self.0
    }

    /// The color of the player who owns the piece.
    pub fn color(self) -> Color {
        self.1
    }

    /// Converts this colored piece into a single character.
    ///
    /// The character is similar to `Piece::to_char`, but with case set based on the player, with
    /// white being uppercase and black being lowercase.
    pub fn to_char(self) -> char {
        match self.color() {
            Color::White => self.piece().to_char().to_ascii_uppercase(),
            Color::Black => self.piece().to_char().to_ascii_lowercase(),
        }
    }

    /// Parses a colored piece from the given character.
    ///
    /// The piece is parsed directly from the character, and the color is determined based on the
    /// case of the character, with white being uppercase and black being lowercase.
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

/// Ranks (rows) of a chessboard.
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
    /// The offset at which this rank starts on a bitboard.
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

    /// Converts the rank to the numeric character that corresponds to it.
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

    /// Parses a rank from the numeric character that corresponds to it, if any.
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

    /// The rank that is immediately "north" ("up" from White's perspective) of this one.
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

    /// The rank that is immediately "south" ("down" from White's perspective) of this one.
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

/// Files (columns) of a chessboard.
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
    /// The offset at which this file starts on a bitboard.
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

    /// Converts the file to the numeric character that corresponds to it.
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

    /// Parses a file from the numeric character that corresponds to it, if any.
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

    /// The file that is immediately "east" ("right" from White's perspective) of this one.
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

    /// The file that is immediately "west" ("left" from White's perspective) of this one.
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

/// A combined rank and file, identifying a square on a chessboard.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RankFile(pub Rank, pub File);

impl RankFile {
    /// The rank of this square.
    pub fn rank(self) -> Rank {
        self.0
    }

    /// The file of this square.
    pub fn file(self) -> File {
        self.1
    }

    /// The index of this square on a bitboard.
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

/// The orthogonal and diagonal directions on a chessboard.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Enum)]
pub enum Direction {
    /// North ("up" from White's perspective).
    North,

    /// South ("down" from White's perspective).
    South,

    /// East ("right" from White's perspective).
    East,

    /// West ("left" from White's perspective).
    West,

    /// North-east.
    NorthEast,

    /// North-west.
    NorthWest,

    /// South-east.
    SouthEast,

    /// South-west.
    SouthWest,
}

impl Direction {
    /// The direction that is directly opposite of this one.
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

    /// The direction in which pawns can move for the given color.
    pub fn forward(color: Color) -> Direction {
        match color {
            Color::White => Direction::North,
            Color::Black => Direction::South,
        }
    }

    /// The direction in which pawns can capture eastward for the given color.
    pub fn forward_east(color: Color) -> Direction {
        match color {
            Color::White => Direction::NorthEast,
            Color::Black => Direction::SouthEast,
        }
    }

    /// The direction in which pawns can capture westward for the given color.
    pub fn forward_west(color: Color) -> Direction {
        match color {
            Color::White => Direction::NorthWest,
            Color::Black => Direction::SouthWest,
        }
    }
}

/// The 8 "directions" in which a knight can move.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Enum)]
pub enum KnightDirection {
    /// Two squares north, one square east.
    NorthNorthEast,

    /// One square north, two squares east.
    NorthEastEast,

    /// Two squares north, one square west.
    NorthNorthWest,

    /// One square north, two squares west.
    NorthWestWest,

    /// Two squares south, one square east.
    SouthSouthEast,

    /// One square south, two squares east.
    SouthEastEast,

    /// Two squares south, one square west.
    SouthSouthWest,

    /// One square south, two squares west.
    SouthWestWest,
}

impl KnightDirection {
    /// The direction that is directly opposite of this one.
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

/// A move, identified by the source square, the target square, and the promotion piece (if any).
/// Castling moves are encoded by the king moving two squares in the direction of the corresponding
/// rook.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Move {
    /// The location of the piece before the move.
    pub source: RankFile,

    /// The location of the piece after the move.
    pub target: RankFile,

    /// What piece this pawn promotes to, if applicable.
    pub promotion: Option<Piece>,
}

impl fmt::Display for Move {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}{}", self.source, self.target)?;
        if let Some(promotion) = self.promotion {
            write!(f, "{}", promotion.to_char().to_ascii_lowercase())?;
        }
        Ok(())
    }
}

impl FromStr for Move {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let split_1 = s
            .char_indices()
            .nth(2)
            .ok_or_else(|| format!("not enough characters"))?
            .0;

        let split_2 = s.char_indices().nth(4).map(|(i, _c)| i).unwrap_or(s.len());
        let source = s[..split_1].parse()?;
        let target = s[split_1..split_2].parse()?;
        let promotion = match &s[split_2..] {
            "" => None,
            other => Some(other.parse()?),
        };
        Ok(Move {
            source,
            target,
            promotion,
        })
    }
}

/// A `Move` with source and target encoded as `Bitboard`s.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BitMove {
    /// The location of the piece before the move.
    pub source: Bitboard,

    /// The location of the piece after the move.
    pub target: Bitboard,

    /// What piece this pawn promotes to, if applicable.
    pub promotion: Option<Piece>,
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
            promotion: move_.promotion,
        }
    }
}

impl From<BitMove> for Move {
    fn from(move_: BitMove) -> Move {
        Move {
            source: move_.source.ls1b().expect("empty source in bitmove"),
            target: move_.target.ls1b().expect("empty target in bitmove"),
            promotion: move_.promotion,
        }
    }
}
