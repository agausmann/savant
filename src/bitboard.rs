use crate::types::{Color, Rank};
use std::ops;

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

    pub fn shift_south(self) -> Bitboard {
        Bitboard(self.0 >> 8)
    }

    pub fn shift_north(self) -> Bitboard {
        Bitboard(self.0 << 8)
    }

    pub fn shift_east(self) -> Bitboard {
        Bitboard((self.0 & NOT_H) << 1)
    }

    pub fn shift_west(self) -> Bitboard {
        Bitboard((self.0 & NOT_A) >> 1)
    }

    pub fn shift_north_east(self) -> Bitboard {
        Bitboard((self.0 & NOT_H) << 9)
    }

    pub fn shift_north_west(self) -> Bitboard {
        Bitboard((self.0 & NOT_A) << 7)
    }

    pub fn shift_south_east(self) -> Bitboard {
        Bitboard((self.0 & NOT_H) >> 7)
    }

    pub fn shift_south_west(self) -> Bitboard {
        Bitboard((self.0 & NOT_A) >> 9)
    }

    pub fn shift_forward(self, color: Color) -> Bitboard {
        match color {
            Color::White => self.shift_north(),
            Color::Black => self.shift_south(),
        }
    }

    pub fn shift_forward_east(self, color: Color) -> Bitboard {
        match color {
            Color::White => self.shift_north_east(),
            Color::Black => self.shift_south_east(),
        }
    }

    pub fn shift_forward_west(self, color: Color) -> Bitboard {
        match color {
            Color::White => self.shift_north_west(),
            Color::Black => self.shift_south_west(),
        }
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
