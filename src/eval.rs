use crate::bitboard::Bitboard;
use crate::position::Position;
use crate::types::Piece;
use enum_map::{enum_map, EnumMap};

/// A fixed-point score value on the centipawn scale.
pub type Score = i64;

pub trait Evaluation {
    fn evaluate(&self, position: &Position) -> Score;
}

/// Material evaluation using fixed point values for each piece type.
#[derive(Debug, Clone)]
pub struct PieceValue {
    map: EnumMap<Piece, Score>,
}

impl PieceValue {
    pub fn new(
        pawn: Score,
        knight: Score,
        bishop: Score,
        rook: Score,
        queen: Score,
        king: Score,
    ) -> Self {
        Self {
            map: enum_map! {
                Piece::Pawn => pawn,
                Piece::Knight => knight,
                Piece::Bishop => bishop,
                Piece::Rook => rook,
                Piece::Queen => queen,
                Piece::King => king,
            },
        }
    }

    pub fn eval(&self, piece: Piece) -> Score {
        self.map[piece]
    }

    pub fn eval_pieces(&self, pieces: &EnumMap<Piece, Bitboard>) -> Score {
        pieces
            .iter()
            .map(|(piece, board)| self.map[piece] * Score::from(board.population_count()))
            .sum()
    }

    pub fn material_balance(
        &self,
        this: &EnumMap<Piece, Bitboard>,
        enemy: &EnumMap<Piece, Bitboard>,
    ) -> Score {
        self.eval_pieces(this) - self.eval_pieces(enemy)
    }
}

impl Evaluation for PieceValue {
    fn evaluate(&self, position: &Position) -> Score {
        self.material_balance(
            &position.pieces_separate(position.next_move()),
            &position.pieces_separate(position.next_move().enemy()),
        )
    }
}
