use crate::bitboard::Bitboard;
use crate::types::{Color, Piece, Rank};
use enum_map::EnumMap;

pub struct Position {
    pieces: EnumMap<Color, EnumMap<Piece, Bitboard>>,
    en_passant: Bitboard,
}

impl Position {
    pub fn occupied_squares(&self) -> Bitboard {
        self.pieces
            .values()
            .flat_map(|per_color| per_color.values())
            .copied()
            .fold(Bitboard::empty(), |acc, item| acc | item)
    }

    pub fn empty_squares(&self) -> Bitboard {
        !self.occupied_squares()
    }

    pub fn pieces(&self, color: Color) -> Bitboard {
        self.pieces[color]
            .values()
            .copied()
            .fold(Bitboard::empty(), |acc, item| acc | item)
    }

    pub fn single_push_targets(&self, color: Color) -> Bitboard {
        self.pieces[color][Piece::Pawn].shift_forward(color) & self.empty_squares()
    }

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

    pub fn pawn_east_attacks(&self, color: Color) -> Bitboard {
        self.pieces[color][Piece::Pawn].shift_forward_east(color)
    }

    pub fn pawn_west_attacks(&self, color: Color) -> Bitboard {
        self.pieces[color][Piece::Pawn].shift_forward_west(color)
    }

    pub fn pawn_east_captures(&self, color: Color) -> Bitboard {
        self.pawn_east_attacks(color) & (self.pieces(color.enemy()) | self.en_passant)
    }

    pub fn pawn_west_captures(&self, color: Color) -> Bitboard {
        self.pawn_west_attacks(color) & (self.pieces(color.enemy()) | self.en_passant)
    }

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

    pub fn king_attacks(&self, color: Color) -> Bitboard {
        let king = self.pieces[color][Piece::King];
        let east_west = king.shift_east() | king.shift_west();

        east_west | (east_west | king).shift_north() | (east_west | king).shift_south()
    }
}
