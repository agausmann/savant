use crate::bitboard::Bitboard;
use crate::types::{
    BitMove, Color, ColoredPiece, Direction, File, KnightDirection, Piece, Rank, RankFile,
};
use enum_map::EnumMap;
use std::str::FromStr;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Position {
    pieces: EnumMap<Color, EnumMap<Piece, Bitboard>>,
    next_move: Color,
    kingside_castle: EnumMap<Color, bool>,
    queenside_castle: EnumMap<Color, bool>,
    en_passant: Bitboard,
    half_move_clock: u16,
    full_move: u16,
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

    pub fn en_passant_targets(&self) -> Bitboard {
        self.en_passant
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
        let target_rank = match color {
            Color::White => Bitboard::rank(Rank::R4),
            Color::Black => Bitboard::rank(Rank::R5),
        };
        self.single_push_targets(color).shift_forward(color) & self.empty_squares() & target_rank
    }

    /// The squares which are [attacked by pawns] of the given color in the eastern direction.
    ///
    /// [attacked by pawns]: https://www.chessprogramming.org/Pawn_Attacks_(Bitboards)
    pub fn pawn_east_attacks(&self, color: Color) -> Bitboard {
        self.pieces[color][Piece::Pawn].pawn_east_attacks(color)
    }

    /// The squares which are [attacked by pawns] of the given color in the western direction.
    ///
    /// [attacked by pawns]: https://www.chessprogramming.org/Pawn_Attacks_(Bitboards)
    pub fn pawn_west_attacks(&self, color: Color) -> Bitboard {
        self.pieces[color][Piece::Pawn].pawn_west_attacks(color)
    }

    /// The squares which contain pieces that can be [captured by pawns] of the given color in the
    /// eastern direction.
    ///
    /// [captured by pawns]: https://www.chessprogramming.org/Pawn_Attacks_(Bitboards)
    pub fn pawn_east_captures(&self, color: Color) -> Bitboard {
        self.pawn_east_attacks(color) & (self.pieces(color.enemy()) | self.en_passant_targets())
    }

    /// The squares which contain pieces that can be [captured by pawns] of the given color in the
    /// western direction.
    ///
    /// [captured by pawns]: https://www.chessprogramming.org/Pawn_Attacks_(Bitboards)
    pub fn pawn_west_captures(&self, color: Color) -> Bitboard {
        self.pawn_west_attacks(color) & (self.pieces(color.enemy()) | self.en_passant_targets())
    }

    /// The squares which are [attacked by knights] of the given color.
    ///
    /// [attacked by knights]: https://www.chessprogramming.org/Knight_Pattern
    pub fn knight_attacks(&self, color: Color) -> Bitboard {
        self.pieces[color][Piece::Knight].knight_attacks()
    }

    /// The squares which are [attacked by the king] of the given color.
    ///
    /// [attacked by the king]: https://www.chessprogramming.org/King_Pattern
    pub fn king_attacks(&self, color: Color) -> Bitboard {
        self.pieces[color][Piece::King].king_attacks()
    }

    pub fn make_move(&mut self, bit_move: BitMove) {
        if !(bit_move.source & self.pieces[self.next_move][Piece::Pawn]).is_empty()
            && !(bit_move.target & Bitboard::rank(self.next_move.double_push_rank())).is_empty()
        {
            self.en_passant = bit_move.source.shift_forward(self.next_move);
        } else {
            self.en_passant = Bitboard::empty();
        }

        let kingside_source = Bitboard::new(0x90 << self.next_move.back_rank().bitboard_offset());
        let queenside_source = Bitboard::new(0x11 << self.next_move.back_rank().bitboard_offset());
        self.kingside_castle[self.next_move] &= (kingside_source & bit_move.source).is_empty();
        self.queenside_castle[self.next_move] &= (queenside_source & bit_move.source).is_empty();

        if (bit_move.source & self.pieces[self.next_move][Piece::Pawn]).is_empty()
            && (bit_move.target & self.pieces(self.next_move.enemy())).is_empty()
        {
            self.half_move_clock += 1;
        } else {
            self.half_move_clock = 0;
        }

        if self.next_move == Color::Black {
            self.full_move += 1;
        }

        for self_pieces in self.pieces[self.next_move].values_mut() {
            if !(bit_move.source & *self_pieces).is_empty() {
                *self_pieces &= !bit_move.source;
                *self_pieces |= bit_move.target;
            }
        }
        for enemy_pieces in self.pieces[self.next_move.enemy()].values_mut() {
            *enemy_pieces &= !bit_move.target;
        }

        self.next_move = self.next_move.enemy();
    }

    /// Direction-wise Generation of Legal Moves ([DirGolem]).
    ///
    /// [DirGolem]: https://www.chessprogramming.org/DirGolem
    pub fn dir_golem(&self) -> DirGolem {
        //TODO test

        let color = self.next_move;

        // Generate enemy attack and in-between sets to detect pinned pieces and king taboo
        // squares.
        let mut horizontal_in_between = Bitboard::empty();
        let mut vertical_in_between = Bitboard::empty();
        let mut diagonal_in_between = Bitboard::empty();
        let mut antidiag_in_between = Bitboard::empty();
        let mut any_attacks = Bitboard::empty();
        let mut orthogonal_super_attacks = Bitboard::empty();
        let mut diagonal_super_attacks = Bitboard::empty();

        let king_xray = self.pieces(color) ^ self.pieces[color][Piece::King];

        let orthogonals =
            self.pieces[color.enemy()][Piece::Rook] | self.pieces[color.enemy()][Piece::Queen];

        // North
        let tmp_slide_attacks = orthogonals.sliding_attacks(king_xray, Direction::North);
        let tmp_king_attacks = self.pieces[color][Piece::King]
            .sliding_attacks(self.occupied_squares(), Direction::South);
        any_attacks |= tmp_slide_attacks;
        orthogonal_super_attacks |= tmp_king_attacks;
        vertical_in_between |= tmp_slide_attacks & tmp_king_attacks;

        // South
        let tmp_slide_attacks = orthogonals.sliding_attacks(king_xray, Direction::South);
        let tmp_king_attacks = self.pieces[color][Piece::King]
            .sliding_attacks(self.occupied_squares(), Direction::North);
        any_attacks |= tmp_slide_attacks;
        orthogonal_super_attacks |= tmp_king_attacks;
        vertical_in_between |= tmp_slide_attacks & tmp_king_attacks;

        // East
        let tmp_slide_attacks = orthogonals.sliding_attacks(king_xray, Direction::East);
        let tmp_king_attacks = self.pieces[color][Piece::King]
            .sliding_attacks(self.occupied_squares(), Direction::West);
        any_attacks |= tmp_slide_attacks;
        orthogonal_super_attacks |= tmp_king_attacks;
        horizontal_in_between |= tmp_slide_attacks & tmp_king_attacks;

        // West
        let tmp_slide_attacks = orthogonals.sliding_attacks(king_xray, Direction::West);
        let tmp_king_attacks = self.pieces[color][Piece::King]
            .sliding_attacks(self.occupied_squares(), Direction::East);
        any_attacks |= tmp_slide_attacks;
        orthogonal_super_attacks |= tmp_king_attacks;
        horizontal_in_between |= tmp_slide_attacks & tmp_king_attacks;

        let diagonals =
            self.pieces[color.enemy()][Piece::Bishop] | self.pieces[color.enemy()][Piece::Queen];

        // NorthEast
        let tmp_slide_attacks = diagonals.sliding_attacks(king_xray, Direction::NorthEast);
        let tmp_king_attacks = self.pieces[color][Piece::King]
            .sliding_attacks(self.occupied_squares(), Direction::SouthWest);
        any_attacks |= tmp_slide_attacks;
        diagonal_super_attacks |= tmp_king_attacks;
        diagonal_in_between |= tmp_slide_attacks & tmp_king_attacks;

        // NorthWest
        let tmp_slide_attacks = diagonals.sliding_attacks(king_xray, Direction::NorthWest);
        let tmp_king_attacks = self.pieces[color][Piece::King]
            .sliding_attacks(self.occupied_squares(), Direction::SouthEast);
        any_attacks |= tmp_slide_attacks;
        diagonal_super_attacks |= tmp_king_attacks;
        antidiag_in_between |= tmp_slide_attacks & tmp_king_attacks;

        // SouthEast
        let tmp_slide_attacks = diagonals.sliding_attacks(king_xray, Direction::SouthEast);
        let tmp_king_attacks = self.pieces[color][Piece::King]
            .sliding_attacks(self.occupied_squares(), Direction::NorthWest);
        any_attacks |= tmp_slide_attacks;
        diagonal_super_attacks |= tmp_king_attacks;
        antidiag_in_between |= tmp_slide_attacks & tmp_king_attacks;

        // SouthWest
        let tmp_slide_attacks = diagonals.sliding_attacks(king_xray, Direction::SouthWest);
        let tmp_king_attacks = self.pieces[color][Piece::King]
            .sliding_attacks(self.occupied_squares(), Direction::NorthEast);
        any_attacks |= tmp_slide_attacks;
        diagonal_super_attacks |= tmp_king_attacks;
        diagonal_in_between |= tmp_slide_attacks & tmp_king_attacks;

        // Non-sliding pieces
        any_attacks |= self.pawn_east_attacks(color.enemy());
        any_attacks |= self.pawn_west_attacks(color.enemy());
        any_attacks |= self.knight_attacks(color.enemy());
        any_attacks |= self.king_attacks(color.enemy());

        // Move generation for current player.

        // Tests/masks for if player is in check.
        let all_in_between =
            horizontal_in_between | vertical_in_between | diagonal_in_between | antidiag_in_between;
        let blocks = all_in_between & self.occupied_squares();
        let check_from = (orthogonal_super_attacks & orthogonals)
            | (diagonal_super_attacks & diagonals)
            | (self.pieces[color][Piece::King].knight_attacks()
                & self.pieces[color.enemy()][Piece::Knight])
            | (self.pieces[color][Piece::King].pawn_attacks(color)
                & self.pieces[color.enemy()][Piece::Pawn]);

        let null_if_check = if (any_attacks & self.pieces[color][Piece::King]).is_empty() {
            Bitboard::universe()
        } else {
            Bitboard::empty()
        };
        let null_if_double_check = if check_from.population_count() < 2 {
            Bitboard::universe()
        } else {
            Bitboard::empty()
        };

        let check_to = check_from | blocks | null_if_check;
        let target_mask = !self.pieces(color) & check_to & null_if_double_check;

        let mut result = DirGolem {
            position: self,
            cardinals: EnumMap::new(),
            knights: EnumMap::new(),
        };

        // Sliding pieces
        let self_orthogonals = self.pieces[color][Piece::Rook] | self.pieces[color][Piece::Queen];
        let self_diagonals = self.pieces[color][Piece::Bishop] | self.pieces[color][Piece::Queen];
        let unpinned_horizontals = self_orthogonals & !(all_in_between ^ horizontal_in_between);
        let unpinned_verticals = self_orthogonals & !(all_in_between ^ vertical_in_between);
        let unpinned_diagonals = self_diagonals & !(all_in_between ^ diagonal_in_between);
        let unpinned_antidiags = self_diagonals & !(all_in_between ^ antidiag_in_between);

        result.cardinals[Direction::North] = unpinned_verticals
            .sliding_attacks(self.occupied_squares(), Direction::North)
            & target_mask;
        result.cardinals[Direction::South] = unpinned_verticals
            .sliding_attacks(self.occupied_squares(), Direction::South)
            & target_mask;
        result.cardinals[Direction::East] = unpinned_horizontals
            .sliding_attacks(self.occupied_squares(), Direction::East)
            & target_mask;
        result.cardinals[Direction::West] = unpinned_horizontals
            .sliding_attacks(self.occupied_squares(), Direction::West)
            & target_mask;
        result.cardinals[Direction::NorthEast] = unpinned_diagonals
            .sliding_attacks(self.occupied_squares(), Direction::NorthEast)
            & target_mask;
        result.cardinals[Direction::NorthWest] = unpinned_antidiags
            .sliding_attacks(self.occupied_squares(), Direction::NorthWest)
            & target_mask;
        result.cardinals[Direction::SouthEast] = unpinned_antidiags
            .sliding_attacks(self.occupied_squares(), Direction::SouthEast)
            & target_mask;
        result.cardinals[Direction::SouthWest] = unpinned_diagonals
            .sliding_attacks(self.occupied_squares(), Direction::SouthWest)
            & target_mask;

        // Knights
        let knights = self.pieces[color][Piece::Knight] & !all_in_between;

        result.knights[KnightDirection::NorthNorthEast] =
            knights.knight_shift(KnightDirection::NorthNorthEast) & target_mask;
        result.knights[KnightDirection::NorthEastEast] =
            knights.knight_shift(KnightDirection::NorthEastEast) & target_mask;
        result.knights[KnightDirection::NorthNorthWest] =
            knights.knight_shift(KnightDirection::NorthNorthWest) & target_mask;
        result.knights[KnightDirection::NorthWestWest] =
            knights.knight_shift(KnightDirection::NorthWestWest) & target_mask;
        result.knights[KnightDirection::SouthSouthEast] =
            knights.knight_shift(KnightDirection::SouthSouthEast) & target_mask;
        result.knights[KnightDirection::SouthEastEast] =
            knights.knight_shift(KnightDirection::SouthEastEast) & target_mask;
        result.knights[KnightDirection::SouthSouthWest] =
            knights.knight_shift(KnightDirection::SouthSouthWest) & target_mask;
        result.knights[KnightDirection::SouthWestWest] =
            knights.knight_shift(KnightDirection::SouthWestWest) & target_mask;

        // Pawn captures
        let pawn_targets = (self.pieces(color.enemy()) & target_mask) | self.en_passant_targets();
        let (east_in_between, west_in_between) = match color {
            Color::White => (diagonal_in_between, antidiag_in_between),
            Color::Black => (antidiag_in_between, diagonal_in_between),
        };
        let diagonal_pawns = self.pieces[color][Piece::Pawn] & !(all_in_between ^ east_in_between);
        result.cardinals[Direction::forward_east(color)] |=
            diagonal_pawns.shift_forward_east(color) & pawn_targets;

        let antidiag_pawns = self.pieces[color][Piece::Pawn] & !(all_in_between ^ west_in_between);
        result.cardinals[Direction::forward_west(color)] |=
            antidiag_pawns.shift_forward_west(color) & pawn_targets;

        // Pawn pushes
        let push_pawns = self.pieces[color][Piece::Pawn] & !(all_in_between ^ vertical_in_between);

        let single_pushes = push_pawns.shift_forward(color) & self.empty_squares();
        result.cardinals[Direction::forward(color)] |= single_pushes;

        let double_pushes = single_pushes.shift_forward(color)
            & self.empty_squares()
            & Bitboard::rank(color.double_push_rank());
        result.cardinals[Direction::forward(color)] |= double_pushes;

        // King

        // king may not move to squares attacked by the enemy or to squares of its own color.
        let king_target_mask = !(self.pieces(color) | any_attacks);

        result.cardinals[Direction::North] |=
            self.pieces[color][Piece::King].shift(Direction::North) & king_target_mask;
        result.cardinals[Direction::South] |=
            self.pieces[color][Piece::King].shift(Direction::South) & king_target_mask;
        result.cardinals[Direction::East] |=
            self.pieces[color][Piece::King].shift(Direction::East) & king_target_mask;
        result.cardinals[Direction::West] |=
            self.pieces[color][Piece::King].shift(Direction::West) & king_target_mask;
        result.cardinals[Direction::NorthEast] |=
            self.pieces[color][Piece::King].shift(Direction::NorthEast) & king_target_mask;
        result.cardinals[Direction::NorthWest] |=
            self.pieces[color][Piece::King].shift(Direction::NorthWest) & king_target_mask;
        result.cardinals[Direction::SouthEast] |=
            self.pieces[color][Piece::King].shift(Direction::SouthEast) & king_target_mask;
        result.cardinals[Direction::SouthWest] |=
            self.pieces[color][Piece::King].shift(Direction::SouthWest) & king_target_mask;

        // Castling

        // vacancies: the squares where pieces must not be present.
        let kingside_vacancies = Bitboard::new(0b01100000 << color.back_rank().bitboard_offset());
        let queenside_vacancies = Bitboard::new(0b00001110 << color.back_rank().bitboard_offset());

        // vulns: the squares which must not be attacked by enemies.
        let kingside_vulns = Bitboard::new(0b01110000 << color.back_rank().bitboard_offset());
        let queenside_vulns = Bitboard::new(0b00011100 << color.back_rank().bitboard_offset());

        // target: the destination square of the king.
        let kingside_target = Bitboard::new(0b01000000 << color.back_rank().bitboard_offset());
        let queenside_target = Bitboard::new(0b00000100 << color.back_rank().bitboard_offset());

        let kingside_mask = if self.kingside_castle[color]
            & (self.occupied_squares() & kingside_vacancies).is_empty()
            & (any_attacks & kingside_vulns).is_empty()
        {
            Bitboard::universe()
        } else {
            Bitboard::empty()
        };
        let queenside_mask = if self.queenside_castle[color]
            & (self.occupied_squares() & queenside_vacancies).is_empty()
            & (any_attacks & queenside_vulns).is_empty()
        {
            Bitboard::universe()
        } else {
            Bitboard::empty()
        };

        result.cardinals[Direction::East] |= kingside_target & kingside_mask;
        result.cardinals[Direction::West] |= queenside_target & queenside_mask;

        result
    }
}

impl FromStr for Position {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut result = Position {
            pieces: EnumMap::new(),
            next_move: Color::White,
            en_passant: Bitboard::empty(),
            kingside_castle: EnumMap::new(),
            queenside_castle: EnumMap::new(),
            half_move_clock: 0,
            full_move: 0,
        };

        let mut parts = s.split_whitespace();

        let board = parts
            .next()
            .ok_or_else(|| format!("unexpected end of string"))?;

        let mut maybe_rank = Some(Rank::R8);
        for rank_str in board.split('/') {
            let rank = maybe_rank.ok_or_else(|| format!("too many ranks given"))?;

            let mut maybe_file = Some(File::Fa);
            for square in rank_str.chars() {
                let file = maybe_file.ok_or_else(|| format!("too many squares given"))?;

                if let Some(ColoredPiece(piece, color)) = ColoredPiece::from_char(square) {
                    result.pieces[color][piece] |= Bitboard::square(RankFile(rank, file));
                    maybe_file = file.east();
                } else if let Some(x) = square.to_digit(10) {
                    for _ in 0..x {
                        maybe_file = maybe_file.and_then(|file| file.east());
                    }
                } else {
                    return Err(format!("unknown square specifier {:?}", square));
                }
            }
            maybe_rank = rank.south();
        }

        let next_move = parts
            .next()
            .ok_or_else(|| format!("unexpected end of string"))?;
        result.next_move = next_move.parse()?;

        let castling_rights = parts
            .next()
            .ok_or_else(|| format!("unexpected end of string"))?;
        if castling_rights != "-" {
            for c in castling_rights.chars() {
                match c {
                    'K' => result.kingside_castle[Color::White] = true,
                    'Q' => result.queenside_castle[Color::White] = true,
                    'k' => result.kingside_castle[Color::Black] = true,
                    'q' => result.queenside_castle[Color::Black] = true,
                    _ => return Err(format!("unexpected flag in castling rights: `{}`", c)),
                }
            }
        }

        let en_passant = parts
            .next()
            .ok_or_else(|| format!("unexpected end of string"))?;
        if en_passant != "-" {
            result.en_passant = Bitboard::square(en_passant.parse()?);
        }

        let half_move_clock = parts.next().unwrap_or("0");
        result.half_move_clock = half_move_clock.parse().map_err(|e| format!("{}", e))?;

        let full_move = parts.next().unwrap_or("1");
        result.full_move = full_move.parse().map_err(|e| format!("{}", e))?;

        Ok(result)
    }
}

#[derive(Debug, Clone)]
pub struct DirGolem<'a> {
    position: &'a Position,
    cardinals: EnumMap<Direction, Bitboard>,
    knights: EnumMap<KnightDirection, Bitboard>,
}

impl<'a> DirGolem<'a> {
    pub fn iter<'b>(&'b self) -> impl Iterator<Item = BitMove> + 'b {
        let cardinals = self.cardinals.iter().flat_map(|(dir, &targets)| {
            targets.into_iter().map(move |target| {
                let source = target.shift(dir.opposite());
                BitMove { source, target }
            })
        });
        let knights = self.knights.iter().flat_map(|(dir, &targets)| {
            targets.into_iter().map(move |target| {
                let source = target.knight_shift(dir.opposite());
                BitMove { source, target }
            })
        });

        cardinals.chain(knights)
    }

    pub fn move_ordered(self, color: Color) -> MoveOrderedDirGolem<'a> {
        let captures = DirGolem {
            position: self.position,
            cardinals: EnumMap::from(|dir| {
                self.cardinals[dir] & self.position.pieces(color.enemy())
            }),
            knights: EnumMap::from(|dir| self.knights[dir] & self.position.pieces(color.enemy())),
        };
        let others = DirGolem {
            position: self.position,
            cardinals: EnumMap::from(|dir| self.cardinals[dir] & !captures.cardinals[dir]),
            knights: EnumMap::from(|dir| self.knights[dir] & !captures.knights[dir]),
        };
        MoveOrderedDirGolem { captures, others }
    }
}

impl<'a> Iterator for DirGolem<'a> {
    type Item = BitMove;

    fn next(&mut self) -> Option<Self::Item> {
        for (direction, bits) in &mut self.cardinals {
            if let Some(target) = bits.next() {
                let source = bits.scan_ray(
                    self.position.pieces(self.position.next_move),
                    direction.opposite(),
                );
                return Some(BitMove { source, target });
            }
        }
        for (direction, bits) in &mut self.knights {
            if let Some(target) = bits.next() {
                let source = bits.knight_shift(direction.opposite());
                return Some(BitMove { source, target });
            }
        }
        None
    }
}

#[derive(Debug, Clone)]
pub struct MoveOrderedDirGolem<'a> {
    captures: DirGolem<'a>,
    others: DirGolem<'a>,
}

impl<'a> Iterator for MoveOrderedDirGolem<'a> {
    type Item = BitMove;

    fn next(&mut self) -> Option<Self::Item> {
        self.captures.next().or_else(|| self.others.next())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // example move sequence from the starting position
    const POSITION_INITIAL: &str = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";
    const POSITION_1_E4: &str = "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1";
    const POSITION_1_C5: &str = "rnbqkbnr/pp1ppppp/8/2p5/4P3/8/PPPP1PPP/RNBQKBNR w KQkq c6 0 2";
    const POSITION_2_NF3: &str = "rnbqkbnr/pp1ppppp/8/2p5/4P3/5N2/PPPP1PPP/RNBQKB1R b KQkq - 1 2";

    // positions useful for perft, sourced from https://www.chessprogramming.org/Perft_Results
    const PERFT_POSITION_2: &str =
        "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq -";
    const PERFT_POSITION_3: &str = "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - -";
    const PERFT_POSITION_4: &str =
        "r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1";

    fn assert_position(position: &str) {
        let _position: Position = position.parse().unwrap();
    }

    fn assert_move(start: &str, move_: &str, end: &str) {
        let mut position: Position = start.parse().unwrap();
        position.make_move(move_.parse().unwrap());
        assert_eq!(position, end.parse().unwrap());
    }

    fn perft(position: Position, depth: usize) -> usize {
        if depth == 0 {
            return 1;
        }
        position
            .dir_golem()
            .map(|move_| {
                let mut new_position = position.clone();
                new_position.make_move(move_);
                perft(new_position, depth - 1)
            })
            .sum()
    }

    #[test]
    fn parse_initial_position() {
        assert_position(POSITION_INITIAL);
    }

    #[test]
    fn parse_1_e4() {
        assert_position(POSITION_1_E4);
    }

    #[test]
    fn parse_1_e4_c5() {
        assert_position(POSITION_1_C5);
    }

    #[test]
    fn parse_1_e4_c5_2_nf3() {
        assert_position(POSITION_2_NF3);
    }

    #[test]
    fn make_move_1_e4() {
        assert_move(POSITION_INITIAL, "e2e4", POSITION_1_E4);
    }

    #[test]
    fn make_move_1_c5() {
        assert_move(POSITION_1_E4, "c7c5", POSITION_1_C5);
    }

    #[test]
    fn make_move_2_nf3() {
        assert_move(POSITION_1_C5, "g1f3", POSITION_2_NF3);
    }
}
