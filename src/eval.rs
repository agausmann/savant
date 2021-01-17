use crate::bitboard::Bitboard;
use crate::position::Position;
use crate::types::{Color, Direction, Piece};
use enum_map::{enum_map, EnumMap};

/// A fixed-point score value on the centipawn scale.
pub type Score = i64;

pub trait Evaluation {
    /// Evaluate the position from the perspective of the player who is next to move, returning a
    /// score that is positive if the position is in their favor.
    fn evaluate(&self, position: &Position) -> Score;
}

/// Material evaluation using fixed point values for each piece type.
#[derive(Debug, Clone)]
pub struct PieceValue {
    score_per_piece: EnumMap<Piece, Score>,
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
            score_per_piece: enum_map! {
                Piece::Pawn => pawn,
                Piece::Knight => knight,
                Piece::Bishop => bishop,
                Piece::Rook => rook,
                Piece::Queen => queen,
                Piece::King => king,
            },
        }
    }

    pub fn score_per_piece(&self, piece: Piece) -> Score {
        self.score_per_piece[piece]
    }

    fn eval_pieces(&self, pieces: &EnumMap<Piece, Bitboard>) -> Score {
        pieces
            .iter()
            .map(|(piece, board)| {
                self.score_per_piece[piece] * Score::from(board.population_count())
            })
            .sum()
    }
}

impl Evaluation for PieceValue {
    fn evaluate(&self, position: &Position) -> Score {
        self.eval_pieces(&position.pieces_separate(position.next_move()))
            - self.eval_pieces(&position.pieces_separate(position.next_move().enemy()))
    }
}

/// Mobility score that counts the number of moves each player has.
pub struct Mobility {
    score_per_move: Score,
}

impl Mobility {
    pub fn new(score_per_move: Score) -> Self {
        Self { score_per_move }
    }
}

impl Evaluation for Mobility {
    fn evaluate(&self, position: &Position) -> Score {
        let this = position.dir_golem_from(position.next_move()).len() as Score;
        let enemy = position.dir_golem_from(position.next_move().enemy()).len() as Score;
        (this - enemy) * self.score_per_move
    }
}

pub struct PawnStructure {
    doubled_pawns: Score,
    backward_pawns: Score,
    isolated_pawns: Score,
}

impl PawnStructure {
    pub fn new(doubled_pawns: Score, backward_pawns: Score, isolated_pawns: Score) -> Self {
        Self {
            doubled_pawns,
            backward_pawns,
            isolated_pawns,
        }
    }

    fn doubled_pawns(&self, pawns: Bitboard) -> Score {
        let north_fill = pawns.shift(Direction::North).fill(Direction::North);
        let south_fill = pawns.shift(Direction::South).fill(Direction::South);
        (pawns & (north_fill | south_fill)).len() as Score
    }

    fn backward_pawns(&self, color: Color, self_pawns: Bitboard, enemy_pawns: Bitboard) -> Score {
        let self_stops = self_pawns.shift_forward(color);

        let self_attackspans = self_pawns
            .pawn_attacks(color)
            .fill(Direction::forward(color));

        let enemy_attacks = enemy_pawns.pawn_attacks(color);

        (self_stops & enemy_attacks & !self_attackspans).len() as Score
    }

    fn isolated_pawns(&self, pawns: Bitboard) -> Score {
        let attack_file_fill = (pawns.shift(Direction::East) | pawns.shift(Direction::West))
            .fill(Direction::North)
            .fill(Direction::South);

        (pawns & !attack_file_fill).len() as Score
    }
}

impl Evaluation for PawnStructure {
    fn evaluate(&self, position: &Position) -> Score {
        let self_pawns = position.pieces_separate(position.next_move())[Piece::Pawn];
        let enemy_pawns = position.pieces_separate(position.next_move().enemy())[Piece::Pawn];

        let doubled_pawns = self.doubled_pawns(self_pawns) - self.doubled_pawns(enemy_pawns);
        let backward_pawns = self.backward_pawns(position.next_move(), self_pawns, enemy_pawns)
            - self.backward_pawns(position.next_move().enemy(), enemy_pawns, self_pawns);
        let isolated_pawns = self.isolated_pawns(self_pawns) - self.isolated_pawns(enemy_pawns);

        doubled_pawns * self.doubled_pawns
            + backward_pawns * self.backward_pawns
            + isolated_pawns * self.isolated_pawns
    }
}

/// A basic evaluation function formulated by Claude Shannon:
///
/// ```txt
/// f(p) = 200(K-K')
///        + 9(Q-Q')
///        + 5(R-R')
///        + 3(B-B' + N-N')
///        + 1(P-P')
///        - 0.5(D-D' + S-S' + I-I')
///        + 0.1(M-M') + ...
///
/// KQRBNP = number of kings, queens, rooks, bishops, knights and pawns
/// D,S,I = doubled, backward and isolated pawns
/// M = Mobility (the number of legal moves)
/// ```
pub struct Shannon;

impl Evaluation for Shannon {
    fn evaluate(&self, position: &Position) -> Score {
        PieceValue::new(100, 300, 300, 500, 900, 20000).evaluate(position)
            + PawnStructure::new(-50, -50, -50).evaluate(position)
            + Mobility::new(10).evaluate(position)
    }
}
