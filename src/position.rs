use crate::bitboard::Bitboard;
use crate::types::{Color, Piece, Rank};
use enum_map::EnumMap;

pub struct Position {
    pieces: EnumMap<Color, EnumMap<Piece, Bitboard>>,
    en_passant: Bitboard,
}

impl Position {
    /// The squares that are occupied by a piece.
    pub fn occupied_squares(&self) -> Bitboard {
        // I have to say, the vectorization and loop unrolling optimizations performed by the
        // compiler here are fantastic. I can write idiomatic code using iterators, and there
        // are absolutely _no_ branches or loops in the resulting x86 assembly.
        self.pieces
            .values()
            .flat_map(|per_color| per_color.values())
            .copied()
            .fold(Bitboard::empty(), |acc, item| acc | item)
    }

    /// The squares that are not occupied by a piece; the complement of `occupied_squares`.
    pub fn empty_squares(&self) -> Bitboard {
        !self.occupied_squares()
    }

    /// The squares that are occupied by a piece of the given color.
    pub fn pieces(&self, color: Color) -> Bitboard {
        self.pieces[color]
            .values()
            .copied()
            .fold(Bitboard::empty(), |acc, item| acc | item)
    }

    /// The squares which are the destination of a [single push] by pawns of the given color.
    ///
    /// [single push]: https://www.chessprogramming.org/Pawn_Pushes_(Bitboards)
    pub fn single_push_targets(&self, color: Color) -> Bitboard {
        self.pieces[color][Piece::Pawn].shift_forward(color) & self.empty_squares()
    }

    /// The squares which are the destination of a [double push] by pawns of the given color.
    ///
    /// [double push]: https://www.chessprogramming.org/Pawn_Pushes_(Bitboards)
    pub fn double_push_targets(&self, color: Color) -> Bitboard {
        fn target_rank(color: Color) -> Bitboard {
            match color {
                Color::White => Bitboard::rank(Rank::R4),
                Color::Black => Bitboard::rank(Rank::R5),
            }
        }
        self.single_push_targets(color).shift_forward(color)
            & self.empty_squares()
            & target_rank(color)
    }

    /// The squares which are [attacked by pawns] of the given color in the eastern direction.
    ///
    /// [attacked by pawns]: https://www.chessprogramming.org/Pawn_Attacks_(Bitboards)
    pub fn pawn_east_attacks(&self, color: Color) -> Bitboard {
        self.pieces[color][Piece::Pawn].shift_forward_east(color)
    }

    /// The squares which are [attacked by pawns] of the given color in the western direction.
    ///
    /// [attacked by pawns]: https://www.chessprogramming.org/Pawn_Attacks_(Bitboards)
    pub fn pawn_west_attacks(&self, color: Color) -> Bitboard {
        self.pieces[color][Piece::Pawn].shift_forward_west(color)
    }

    /// The squares which contain pieces that can be [captured by pawns] of the given color in the
    /// eastern direction.
    ///
    /// [captured by pawns]: https://www.chessprogramming.org/Pawn_Attacks_(Bitboards)
    pub fn pawn_east_captures(&self, color: Color) -> Bitboard {
        self.pawn_east_attacks(color) & (self.pieces(color.enemy()) | self.en_passant)
    }

    /// The squares which contain pieces that can be [captured by pawns] of the given color in the
    /// western direction.
    ///
    /// [captured by pawns]: https://www.chessprogramming.org/Pawn_Attacks_(Bitboards)
    pub fn pawn_west_captures(&self, color: Color) -> Bitboard {
        self.pawn_west_attacks(color) & (self.pieces(color.enemy()) | self.en_passant)
    }

    /// The squares which are [attacked by knights] of the given color.
    ///
    /// [attacked by knights]: https://www.chessprogramming.org/Knight_Pattern
    pub fn knight_attacks(&self, color: Color) -> Bitboard {
        let knights = self.pieces[color][Piece::Knight];
        let east_one = knights.shift_east();
        let west_one = knights.shift_west();
        let east_two = east_one.shift_east();
        let west_two = west_one.shift_west();

        (east_two | west_two).shift_north()
            | (east_two | west_two).shift_south()
            | (east_one | west_one).shift_north().shift_north()
            | (east_one | west_one).shift_south().shift_south()
    }

    /// The squares which are [attacked by the king] of the given color.
    ///
    /// [attacked by the king]: https://www.chessprogramming.org/King_Pattern
    pub fn king_attacks(&self, color: Color) -> Bitboard {
        let king = self.pieces[color][Piece::King];
        let east_west = king.shift_east() | king.shift_west();

        east_west | (east_west | king).shift_north() | (east_west | king).shift_south()
    }
}
