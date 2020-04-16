use crate::types::{Color, Direction, Rank};
use std::ops;

const IDENT: u64 = 0xffffffffffffffff;
const NOT_A: u64 = 0xfefefefefefefefe;
const NOT_H: u64 = 0x7f7f7f7f7f7f7f7f;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Bitboard(u64);

impl Bitboard {
    pub fn empty() -> Bitboard {
        Bitboard(0)
    }

    pub fn universe() -> Bitboard {
        !Bitboard::empty()
    }

    pub fn rank(rank: Rank) -> Bitboard {
        Bitboard(0x00000000000000ff << rank.bitboard_offset())
    }

    pub fn shift(self, direction: Direction) -> Bitboard {
        let (offset, mask) = match direction {
            Direction::North => (8, IDENT),
            Direction::South => (64 - 8, IDENT),
            Direction::East => (1, NOT_H),
            Direction::West => (64 - 1, NOT_A),
            Direction::NorthEast => (9, NOT_H),
            Direction::NorthWest => (7, NOT_A),
            Direction::SouthEast => (64 - 7, NOT_H),
            Direction::SouthWest => (64 - 9, NOT_A),
        };
        Bitboard((self.0 & mask).rotate_left(offset))
    }

    pub fn shift_forward(self, color: Color) -> Bitboard {
        match color {
            Color::White => self.shift(Direction::North),
            Color::Black => self.shift(Direction::South),
        }
    }

    pub fn shift_forward_east(self, color: Color) -> Bitboard {
        match color {
            Color::White => self.shift(Direction::NorthEast),
            Color::Black => self.shift(Direction::SouthEast),
        }
    }

    pub fn shift_forward_west(self, color: Color) -> Bitboard {
        match color {
            Color::White => self.shift(Direction::NorthWest),
            Color::Black => self.shift(Direction::SouthWest),
        }
    }

    pub fn occluded_fill(self, empty: Bitboard, direction: Direction) -> Bitboard {
        let mut flood = self;
        let mut slide = self;
        for _ in 0..7 {
            slide = slide.shift(direction) & empty;
            flood |= slide;
        }
        flood
    }

    pub fn sliding_attacks(self, empty: Bitboard, direction: Direction) -> Bitboard {
        self.occluded_fill(empty, direction).shift(direction)
    }
}

impl Default for Bitboard {
    fn default() -> Self {
        Bitboard::empty()
    }
}

impl ops::BitAnd for Bitboard {
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self::Output {
        Bitboard(self.0 & rhs.0)
    }
}

impl ops::BitAndAssign for Bitboard {
    fn bitand_assign(&mut self, rhs: Self) {
        self.0 &= rhs.0;
    }
}

impl ops::BitOr for Bitboard {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        Bitboard(self.0 | rhs.0)
    }
}

impl ops::BitOrAssign for Bitboard {
    fn bitor_assign(&mut self, rhs: Self) {
        self.0 |= rhs.0;
    }
}

impl ops::BitXor for Bitboard {
    type Output = Self;

    fn bitxor(self, rhs: Self) -> Self::Output {
        Bitboard(self.0 ^ rhs.0)
    }
}

impl ops::BitXorAssign for Bitboard {
    fn bitxor_assign(&mut self, rhs: Self) {
        self.0 ^= rhs.0;
    }
}

impl ops::Not for Bitboard {
    type Output = Self;

    fn not(self) -> Self::Output {
        Bitboard(!self.0)
    }
}
