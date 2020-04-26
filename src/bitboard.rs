use crate::types::{Color, Direction, File, KnightDirection, Rank, RankFile};
use std::ops;

const NOT_1: Bitboard = bitboard![
    1 1 1 1 1 1 1 1
    1 1 1 1 1 1 1 1
    1 1 1 1 1 1 1 1
    1 1 1 1 1 1 1 1
    1 1 1 1 1 1 1 1
    1 1 1 1 1 1 1 1
    1 1 1 1 1 1 1 1
    . . . . . . . .
];
const NOT_2: Bitboard = bitboard![
    1 1 1 1 1 1 1 1
    1 1 1 1 1 1 1 1
    1 1 1 1 1 1 1 1
    1 1 1 1 1 1 1 1
    1 1 1 1 1 1 1 1
    1 1 1 1 1 1 1 1
    . . . . . . . .
    1 1 1 1 1 1 1 1
];
const NOT_7: Bitboard = bitboard![
    1 1 1 1 1 1 1 1
    . . . . . . . .
    1 1 1 1 1 1 1 1
    1 1 1 1 1 1 1 1
    1 1 1 1 1 1 1 1
    1 1 1 1 1 1 1 1
    1 1 1 1 1 1 1 1
    1 1 1 1 1 1 1 1
];
const NOT_8: Bitboard = bitboard![
    . . . . . . . .
    1 1 1 1 1 1 1 1
    1 1 1 1 1 1 1 1
    1 1 1 1 1 1 1 1
    1 1 1 1 1 1 1 1
    1 1 1 1 1 1 1 1
    1 1 1 1 1 1 1 1
    1 1 1 1 1 1 1 1
];
const NOT_A: Bitboard = bitboard![
    . 1 1 1 1 1 1 1
    . 1 1 1 1 1 1 1
    . 1 1 1 1 1 1 1
    . 1 1 1 1 1 1 1
    . 1 1 1 1 1 1 1
    . 1 1 1 1 1 1 1
    . 1 1 1 1 1 1 1
    . 1 1 1 1 1 1 1
];
const NOT_B: Bitboard = bitboard![
    1 . 1 1 1 1 1 1
    1 . 1 1 1 1 1 1
    1 . 1 1 1 1 1 1
    1 . 1 1 1 1 1 1
    1 . 1 1 1 1 1 1
    1 . 1 1 1 1 1 1
    1 . 1 1 1 1 1 1
    1 . 1 1 1 1 1 1
];
const NOT_G: Bitboard = bitboard![
    1 1 1 1 1 1 . 1
    1 1 1 1 1 1 . 1
    1 1 1 1 1 1 . 1
    1 1 1 1 1 1 . 1
    1 1 1 1 1 1 . 1
    1 1 1 1 1 1 . 1
    1 1 1 1 1 1 . 1
    1 1 1 1 1 1 . 1
];
const NOT_H: Bitboard = bitboard![
    1 1 1 1 1 1 1 .
    1 1 1 1 1 1 1 .
    1 1 1 1 1 1 1 .
    1 1 1 1 1 1 1 .
    1 1 1 1 1 1 1 .
    1 1 1 1 1 1 1 .
    1 1 1 1 1 1 1 .
    1 1 1 1 1 1 1 .
];

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Bitboard(u64);

impl Bitboard {
    /// Creates a bitboard from a raw integer value.
    ///
    /// # Bit order
    ///
    /// Bits 0..63 are assigned to board squares in row-major order (a1, a2, ..., b1, b2, ...),
    /// starting with a1 at the least-significant bit.
    pub const fn new(x: u64) -> Bitboard {
        Bitboard(x)
    }

    pub const fn empty() -> Bitboard {
        bitboard![
            . . . . . . . .
            . . . . . . . .
            . . . . . . . .
            . . . . . . . .
            . . . . . . . .
            . . . . . . . .
            . . . . . . . .
            . . . . . . . .
        ]
    }

    pub const fn universe() -> Bitboard {
        bitboard![
            1 1 1 1 1 1 1 1
            1 1 1 1 1 1 1 1
            1 1 1 1 1 1 1 1
            1 1 1 1 1 1 1 1
            1 1 1 1 1 1 1 1
            1 1 1 1 1 1 1 1
            1 1 1 1 1 1 1 1
            1 1 1 1 1 1 1 1
        ]
    }

    pub fn rank(rank: Rank) -> Bitboard {
        Bitboard(0x00000000000000ff << rank.bitboard_offset())
    }

    pub fn file(file: File) -> Bitboard {
        Bitboard(0x0101010101010101 << file.bitboard_offset())
    }

    pub fn square(rank_file: RankFile) -> Bitboard {
        Bitboard(0x0000000000000001 << rank_file.bitboard_offset())
    }

    pub fn is_empty(self) -> bool {
        self == Bitboard::empty()
    }

    pub fn population_count(self) -> u32 {
        self.0.count_ones()
    }

    pub fn shift(self, direction: Direction) -> Bitboard {
        let (offset, mask) = match direction {
            Direction::North => (8, NOT_8),
            Direction::South => (64 - 8, NOT_1),
            Direction::East => (1, NOT_H),
            Direction::West => (64 - 1, NOT_A),
            Direction::NorthEast => (9, NOT_8 & NOT_H),
            Direction::NorthWest => (7, NOT_8 & NOT_A),
            Direction::SouthEast => (64 - 7, NOT_1 & NOT_H),
            Direction::SouthWest => (64 - 9, NOT_1 & NOT_A),
        };
        Bitboard((self & mask).0.rotate_left(offset))
    }

    pub fn knight_shift(self, direction: KnightDirection) -> Bitboard {
        let (offset, mask) = match direction {
            KnightDirection::NorthNorthEast => (17, NOT_8 & NOT_7 & NOT_H),
            KnightDirection::NorthEastEast => (10, NOT_8 & NOT_G & NOT_H),
            KnightDirection::NorthNorthWest => (15, NOT_8 & NOT_7 & NOT_A),
            KnightDirection::NorthWestWest => (6, NOT_8 & NOT_B & NOT_A),
            KnightDirection::SouthSouthEast => (64 - 15, NOT_1 & NOT_2 & NOT_H),
            KnightDirection::SouthEastEast => (64 - 6, NOT_1 & NOT_G & NOT_H),
            KnightDirection::SouthSouthWest => (64 - 17, NOT_1 & NOT_2 & NOT_A),
            KnightDirection::SouthWestWest => (64 - 10, NOT_1 & NOT_B & NOT_A),
        };
        Bitboard((self & mask).0.rotate_left(offset))
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

    pub fn sliding_attacks(self, pieces: Bitboard, direction: Direction) -> Bitboard {
        self.occluded_fill(!pieces, direction).shift(direction)
    }

    /// The squares which are [attacked by pawns] of the given color in the eastern direction.
    ///
    /// [attacked by pawns]: https://www.chessprogramming.org/Pawn_Attacks_(Bitboards)
    pub fn pawn_east_attacks(self, color: Color) -> Bitboard {
        self.shift_forward_east(color)
    }

    /// The squares which are [attacked by pawns] of the given color in the western direction.
    ///
    /// [attacked by pawns]: https://www.chessprogramming.org/Pawn_Attacks_(Bitboards)
    pub fn pawn_west_attacks(self, color: Color) -> Bitboard {
        self.shift_forward_west(color)
    }

    pub fn pawn_attacks(self, color: Color) -> Bitboard {
        self.pawn_east_attacks(color) | self.pawn_west_attacks(color)
    }

    /// The squares which are [attacked by knights].
    ///
    /// [attacked by knights]: https://www.chessprogramming.org/Knight_Pattern
    pub fn knight_attacks(self) -> Bitboard {
        let east_one = self.shift(Direction::East);
        let west_one = self.shift(Direction::West);
        let east_two = east_one.shift(Direction::East);
        let west_two = west_one.shift(Direction::West);

        (east_two | west_two).shift(Direction::North)
            | (east_two | west_two).shift(Direction::South)
            | (east_one | west_one)
                .shift(Direction::North)
                .shift(Direction::North)
            | (east_one | west_one)
                .shift(Direction::South)
                .shift(Direction::South)
    }

    /// The squares which are [attacked by the king].
    ///
    /// [attacked by the king]: https://www.chessprogramming.org/King_Pattern
    pub fn king_attacks(self) -> Bitboard {
        let east_west = self.shift(Direction::East) | self.shift(Direction::West);

        east_west
            | (east_west | self).shift(Direction::North)
            | (east_west | self).shift(Direction::South)
    }

    pub fn fill(self, direction: Direction) -> Bitboard {
        self.occluded_fill(Bitboard::universe(), direction)
    }

    pub fn scan_ray(self, targets: Bitboard, direction: Direction) -> Bitboard {
        let mut inline_targets = targets | self.fill(direction);
        match direction {
            Direction::North | Direction::East | Direction::NorthEast | Direction::NorthWest => {
                inline_targets.next().unwrap_or(Bitboard::empty())
            }
            Direction::South | Direction::West | Direction::SouthEast | Direction::SouthWest => {
                inline_targets.next_back().unwrap_or(Bitboard::empty())
            }
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

impl Iterator for Bitboard {
    type Item = Bitboard;

    fn next(&mut self) -> Option<Self::Item> {
        // isolate LS1B
        let bit = self.0 & self.0.wrapping_neg();

        // reset LS1B
        self.0 &= self.0 - 1;

        if bit != 0 {
            Some(Bitboard(bit))
        } else {
            None
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let len = self.population_count() as usize;
        (len, Some(len))
    }
}

impl DoubleEndedIterator for Bitboard {
    fn next_back(&mut self) -> Option<Self::Item> {
        // set all bits below MS1B
        let mut fill = self.0;
        fill |= fill >> 32;
        fill |= fill >> 16;
        fill |= fill >> 8;
        fill |= fill >> 4;
        fill |= fill >> 2;
        fill |= fill >> 1;

        // calculate MS1B
        let bit = (fill >> 1) + 1;

        // reset MS1B
        self.0 &= fill >> 1;

        if bit != 0 {
            Some(Bitboard(bit))
        } else {
            None
        }
    }
}

impl ExactSizeIterator for Bitboard {}
