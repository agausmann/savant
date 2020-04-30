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
        if !(self.pieces[self.next_move][Piece::Pawn] & bit_move.source).is_empty()
            && bit_move.target == self.en_passant
        {
            self.pieces[self.next_move.enemy()][Piece::Pawn] &=
                !(self.en_passant.shift_forward(self.next_move.enemy()));
        }

        // TODO the en passant target must strictly be legal.
        // examples where this is not the case:
        // - en passant not available in discovered check
        //      rnbqkbnr/ppp1pppp/8/3pPK2/8/8/PPPP1PPP/RNBQ1BNR w kq - 0 2
        // - en passant pin
        //      k7/8/8/3KPp1r/8/8/8/8 w - - 0 2
        //
        // potential workaround: allow illegal en passant square in internal state,
        // explicitly check whether en passant is legal in `dir_golem` and FEN serializers.
        let double_push_source =
            Bitboard::rank(self.next_move.back_rank()).shift_forward(self.next_move);
        let double_push_target = Bitboard::rank(self.next_move.double_push_rank());
        let en_passant_sources =
            bit_move.target.shift(Direction::East) | bit_move.target.shift(Direction::West);
        let orthogonals =
            self.pieces[self.next_move][Piece::Rook] | self.pieces[self.next_move][Piece::Queen];
        let in_between = (orthogonals.sliding_attacks(self.occupied_squares(), Direction::East)
            & self.pieces[self.next_move.enemy()][Piece::King]
                .sliding_attacks(self.occupied_squares(), Direction::West))
            | (orthogonals.sliding_attacks(self.occupied_squares(), Direction::West)
                & self.pieces[self.next_move.enemy()][Piece::King]
                    .sliding_attacks(self.occupied_squares(), Direction::East));
        if !(bit_move.source & self.pieces[self.next_move][Piece::Pawn] & double_push_source)
            .is_empty()
            && !(bit_move.target & double_push_target).is_empty()
            && (en_passant_sources & in_between).is_empty()
        {
            self.en_passant = bit_move.source.shift_forward(self.next_move);
        } else {
            self.en_passant = Bitboard::empty();
        }

        let castle_source = Bitboard::back_rank(self.next_move, 0b00010000);
        let kingside_target = Bitboard::back_rank(self.next_move, 0b01000000);
        let queenside_target = Bitboard::back_rank(self.next_move, 0b00000100);
        if !(bit_move.source & self.pieces[self.next_move][Piece::King] & castle_source).is_empty()
        {
            if !(bit_move.target & kingside_target).is_empty() {
                let rook_source = Bitboard::back_rank(self.next_move, 0b10000000);
                let rook_target = Bitboard::back_rank(self.next_move, 0b00100000);
                self.pieces[self.next_move][Piece::Rook] &= !rook_source;
                self.pieces[self.next_move][Piece::Rook] |= rook_target;
            } else if !(bit_move.target & queenside_target).is_empty() {
                let rook_source = Bitboard::back_rank(self.next_move, 0b00000001);
                let rook_target = Bitboard::back_rank(self.next_move, 0b00001000);
                self.pieces[self.next_move][Piece::Rook] &= !rook_source;
                self.pieces[self.next_move][Piece::Rook] |= rook_target;
            }
        }

        let kingside_source = Bitboard::back_rank(self.next_move, 0b10010000);
        let queenside_source = Bitboard::back_rank(self.next_move, 0b00010001);
        self.kingside_castle[self.next_move] &= (kingside_source & bit_move.source).is_empty();
        self.queenside_castle[self.next_move] &= (queenside_source & bit_move.source).is_empty();

        let kingside_target = Bitboard::back_rank(self.next_move.enemy(), 0b10000000);
        let queenside_target = Bitboard::back_rank(self.next_move.enemy(), 0b00000001);
        self.kingside_castle[self.next_move.enemy()] &=
            (kingside_target & bit_move.target).is_empty();
        self.queenside_castle[self.next_move.enemy()] &=
            (queenside_target & bit_move.target).is_empty();

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
        if let Some(promotion) = bit_move.promotion {
            self.pieces[self.next_move][Piece::Pawn] &= !bit_move.target;
            self.pieces[self.next_move][promotion] |= bit_move.target;
        }
        for enemy_pieces in self.pieces[self.next_move.enemy()].values_mut() {
            *enemy_pieces &= !bit_move.target;
        }

        self.next_move = self.next_move.enemy();
    }

    pub fn perft(&self, depth: usize) -> usize {
        if depth == 0 {
            return 1;
        }
        self.dir_golem()
            .map(|move_| {
                let mut new_position = self.clone();
                new_position.make_move(move_);
                new_position.perft(depth - 1)
            })
            .sum()
    }

    /// Direction-wise Generation of Legal Moves ([DirGolem]).
    ///
    /// [DirGolem]: https://www.chessprogramming.org/DirGolem
    pub fn dir_golem(&self) -> DirGolem {
        //TODO test

        let color = self.next_move;

        // In-between sets, calculated by the intersection of sliding attacks in one direction and
        // the king in the opposite direction, for each of the possible move directions.
        //
        // When the king is in check from an enemy piece (zero pieces in between), the in-between
        // set in that direction will be the set of squares in between the king and enemy piece on
        // the rank, file, or diagonal that they share, aka the potential target squares for
        // blocking check.
        //
        // In the case of a single piece in between the king and enemy piece, both sliding attack
        // fills will stop at that piece, and the the in-between set will consist of only that
        // piece, highlighting it as "pinned" in that direction. (or alternatively, if it is an
        // enemy piece, a candidate to move for discovered check).
        //
        // If there are two or more pieces in between the king and enemy piece, both sliding attack
        // fills will stop before they reach each other, and the in-between set will be null.
        let mut horizontal_in_between = Bitboard::empty();
        let mut vertical_in_between = Bitboard::empty();
        let mut diagonal_in_between = Bitboard::empty();
        let mut antidiag_in_between = Bitboard::empty();

        // The set of all squares which are attacked by enemy pieces. These squares are "taboo" for
        // the king, and must be masked out of his potential move set.
        let mut any_attacks = Bitboard::empty();

        // "Super attack" sets for the king, sliding piece scans that originate at the king's
        // position to detect the sliding pieces that are delivering check.
        let mut orthogonal_super_attacks = Bitboard::empty();
        let mut diagonal_super_attacks = Bitboard::empty();

        // The set of all pieces except the friendly king, which is removed to detect squares that
        // are attacked when the king is moved. Otherwise, the king casts a shadow behind him from
        // the perspective of the piece delivering check, which would make moving backward seem
        // like a valid move.
        let king_xray = self.occupied_squares() ^ self.pieces[color][Piece::King];

        // Populate in-between sets and attacks sets with sliding piece moves.
        //
        // Enemy sliding pieces are split into two categories, those that can move along the
        // diagonals and antidiagonals and those that can move along ranks and files.
        let orthogonals =
            self.pieces[color.enemy()][Piece::Rook] | self.pieces[color.enemy()][Piece::Queen];
        let diagonals =
            self.pieces[color.enemy()][Piece::Bishop] | self.pieces[color.enemy()][Piece::Queen];

        // For each direction, generate the attack sets for the corresponding group of enemy
        // pieces, and the super attack set for the king in the opposite direction, and update
        // the cumulative attack, super attack, and in-between sets.
        let mut generate_sliding_attacks =
            |direction: Direction,
             pieces: Bitboard,
             super_attacks: &mut Bitboard,
             in_between: &mut Bitboard| {
                let tmp_slide_attacks = pieces.sliding_attacks(king_xray, direction);
                let tmp_king_super_attacks = self.pieces[color][Piece::King]
                    .sliding_attacks(self.occupied_squares(), direction.opposite());

                any_attacks |= tmp_slide_attacks;
                *super_attacks |= tmp_king_super_attacks;
                *in_between |= tmp_slide_attacks & tmp_king_super_attacks;
            };
        generate_sliding_attacks(
            Direction::North,
            orthogonals,
            &mut orthogonal_super_attacks,
            &mut vertical_in_between,
        );
        generate_sliding_attacks(
            Direction::South,
            orthogonals,
            &mut orthogonal_super_attacks,
            &mut vertical_in_between,
        );
        generate_sliding_attacks(
            Direction::East,
            orthogonals,
            &mut orthogonal_super_attacks,
            &mut horizontal_in_between,
        );
        generate_sliding_attacks(
            Direction::West,
            orthogonals,
            &mut orthogonal_super_attacks,
            &mut horizontal_in_between,
        );
        generate_sliding_attacks(
            Direction::NorthEast,
            diagonals,
            &mut diagonal_super_attacks,
            &mut diagonal_in_between,
        );
        generate_sliding_attacks(
            Direction::NorthWest,
            diagonals,
            &mut diagonal_super_attacks,
            &mut antidiag_in_between,
        );
        generate_sliding_attacks(
            Direction::SouthEast,
            diagonals,
            &mut diagonal_super_attacks,
            &mut antidiag_in_between,
        );
        generate_sliding_attacks(
            Direction::SouthWest,
            diagonals,
            &mut diagonal_super_attacks,
            &mut diagonal_in_between,
        );

        // Union of the resulting in between sets.
        let all_in_between =
            horizontal_in_between | vertical_in_between | diagonal_in_between | antidiag_in_between;

        // Add non-sliding-piece attacks to the cumulative attack set.
        // These can't generate pins or be blocked by other pieces, so they don't affect in-between
        // sets.
        any_attacks |= self.pawn_east_attacks(color.enemy());
        any_attacks |= self.pawn_west_attacks(color.enemy());
        any_attacks |= self.knight_attacks(color.enemy());
        any_attacks |= self.king_attacks(color.enemy());

        // Create a target mask for non-king pieces that accounts for check, double check, and
        // friendly pieces.

        // The set of squares that are a target for blocking, if we are in check.
        let blocks = all_in_between & self.empty_squares();

        // Find the pieces that are currently giving check to the king.
        let check_from = (orthogonal_super_attacks & orthogonals)
            | (diagonal_super_attacks & diagonals)
            | (self.pieces[color][Piece::King].knight_attacks()
                & self.pieces[color.enemy()][Piece::Knight])
            | (self.pieces[color][Piece::King].pawn_attacks(color)
                & self.pieces[color.enemy()][Piece::Pawn]);

        let mut target_mask = !self.pieces(color);

        // If we are in check, only allow blocking moves and capture of the enemy attacker.
        if !check_from.is_empty() {
            target_mask &= check_from | blocks;
        }

        // If we are in double check, no blocking or capturing is allowed, the king must move.
        // Disallow all non-king moves.
        if check_from.population_count() >= 2 {
            target_mask = Bitboard::empty()
        }

        // With the masks above, we are now able to start generating moves.
        let mut result = DirGolem {
            position: self,
            cardinals: EnumMap::new(),
            knights: EnumMap::new(),
            pending_promotion: None,
        };

        // Sliding pieces:
        let self_orthogonals = self.pieces[color][Piece::Rook] | self.pieces[color][Piece::Queen];
        let self_diagonals = self.pieces[color][Piece::Bishop] | self.pieces[color][Piece::Queen];

        // Sets of sliding pieces which are unpinned in the given move direction.
        let unpinned_horizontals = self_orthogonals & !(all_in_between ^ horizontal_in_between);
        let unpinned_verticals = self_orthogonals & !(all_in_between ^ vertical_in_between);
        let unpinned_diagonals = self_diagonals & !(all_in_between ^ diagonal_in_between);
        let unpinned_antidiags = self_diagonals & !(all_in_between ^ antidiag_in_between);

        let mut generate_cardinal_moves = |direction: Direction, sources: Bitboard| {
            result.cardinals[direction] |=
                sources.sliding_attacks(self.occupied_squares(), direction) & target_mask;
        };
        generate_cardinal_moves(Direction::North, unpinned_verticals);
        generate_cardinal_moves(Direction::South, unpinned_verticals);
        generate_cardinal_moves(Direction::East, unpinned_horizontals);
        generate_cardinal_moves(Direction::West, unpinned_horizontals);
        generate_cardinal_moves(Direction::NorthEast, unpinned_diagonals);
        generate_cardinal_moves(Direction::NorthWest, unpinned_antidiags);
        generate_cardinal_moves(Direction::SouthEast, unpinned_antidiags);
        generate_cardinal_moves(Direction::SouthWest, unpinned_diagonals);

        // Knights
        let unpinned_knights = self.pieces[color][Piece::Knight] & !all_in_between;

        let mut generate_knight_moves = |direction: KnightDirection| {
            result.knights[direction] = unpinned_knights.knight_shift(direction) & target_mask;
        };
        generate_knight_moves(KnightDirection::NorthNorthEast);
        generate_knight_moves(KnightDirection::NorthEastEast);
        generate_knight_moves(KnightDirection::NorthNorthWest);
        generate_knight_moves(KnightDirection::NorthWestWest);
        generate_knight_moves(KnightDirection::SouthSouthEast);
        generate_knight_moves(KnightDirection::SouthEastEast);
        generate_knight_moves(KnightDirection::SouthSouthWest);
        generate_knight_moves(KnightDirection::SouthWestWest);

        // Pawn captures

        // Capture targets include enemy pieces
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
        result.cardinals[Direction::forward(color)] |= single_pushes & target_mask;

        let double_pushes = single_pushes.shift_forward(color)
            & self.empty_squares()
            & Bitboard::rank(color.double_push_rank());
        result.cardinals[Direction::forward(color)] |= double_pushes & target_mask;

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
        let kingside_vacancies = Bitboard::back_rank(color, 0b01100000);
        let queenside_vacancies = Bitboard::back_rank(color, 0b00001110);

        // vulns: the squares which must not be attacked by enemies.
        let kingside_vulns = Bitboard::back_rank(color, 0b01110000);
        let queenside_vulns = Bitboard::back_rank(color, 0b00011100);

        // target: the destination square of the king.
        let kingside_target = Bitboard::back_rank(color, 0b01000000);
        let queenside_target = Bitboard::back_rank(color, 0b00000100);

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
    pending_promotion: Option<(Bitboard, Bitboard, Piece)>,
}

impl<'a> DirGolem<'a> {
    pub fn move_ordered(self, color: Color) -> MoveOrderedDirGolem<'a> {
        let captures = DirGolem {
            position: self.position,
            cardinals: EnumMap::from(|dir| {
                self.cardinals[dir] & self.position.pieces(color.enemy())
            }),
            knights: EnumMap::from(|dir| self.knights[dir] & self.position.pieces(color.enemy())),
            pending_promotion: None,
        };
        let others = DirGolem {
            position: self.position,
            cardinals: EnumMap::from(|dir| self.cardinals[dir] & !captures.cardinals[dir]),
            knights: EnumMap::from(|dir| self.knights[dir] & !captures.knights[dir]),
            pending_promotion: None,
        };
        MoveOrderedDirGolem { captures, others }
    }
}

impl<'a> Iterator for DirGolem<'a> {
    type Item = BitMove;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some((source, target, piece)) = self.pending_promotion {
            let next_piece = match piece {
                Piece::Rook => Some(Piece::Bishop),
                Piece::Bishop => Some(Piece::Knight),
                Piece::Knight => None,
                _ => unreachable!(),
            };
            self.pending_promotion = next_piece.map(|piece| (source, target, piece));
            let promotion = Some(piece);
            return Some(BitMove {
                source,
                target,
                promotion,
            });
        }
        for (direction, bits) in &mut self.cardinals {
            if let Some(target) = bits.next() {
                let source = target.scan_ray(
                    self.position.pieces(self.position.next_move),
                    direction.opposite(),
                );
                let promotion = if !(source
                    & self.position.pieces[self.position.next_move][Piece::Pawn])
                    .is_empty()
                    && !(target & Bitboard::rank(self.position.next_move.enemy().back_rank()))
                        .is_empty()
                {
                    self.pending_promotion = Some((source, target, Piece::Rook));
                    Some(Piece::Queen)
                } else {
                    None
                };
                return Some(BitMove {
                    source,
                    target,
                    promotion,
                });
            }
        }
        for (direction, bits) in &mut self.knights {
            if let Some(target) = bits.next() {
                let source = target.knight_shift(direction.opposite());
                return Some(BitMove {
                    source,
                    target,
                    promotion: None,
                });
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

    // Starting positions for perft tests, source: https://www.chessprogramming.org/Perft_Results
    const POSITION_PERFT_2: &str =
        "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq -";
    const POSITION_PERFT_3: &str = "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - -";
    const POSITION_PERFT_4: &str =
        "r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1";
    const POSITION_PERFT_5: &str = "rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8";
    const POSITION_PERFT_6: &str =
        "r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10";

    fn assert_position(position: &str) {
        let _position: Position = position.parse().unwrap();
    }

    fn assert_move(start: &str, move_: &str, end: &str) {
        let mut position: Position = start.parse().unwrap();
        position.make_move(move_.parse().unwrap());
        assert_eq!(position, end.parse().unwrap());
    }

    fn assert_perft(start: &str, depth: usize, count: usize) {
        let position: Position = start.parse().unwrap();
        assert_eq!(position.perft(depth), count);
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

    #[test]
    fn perft_initial() {
        assert_perft(POSITION_INITIAL, 6, 119_060_324);
    }

    #[test]
    fn perft_2() {
        assert_perft(POSITION_PERFT_2, 5, 193_690_690);
    }

    #[test]
    fn perft_3() {
        assert_perft(POSITION_PERFT_3, 7, 178_633_661);
    }

    #[test]
    fn perft_4() {
        assert_perft(POSITION_PERFT_4, 5, 15_833_292);
    }

    #[test]
    fn perft_5() {
        assert_perft(POSITION_PERFT_5, 5, 89_941_194);
    }

    #[test]
    fn perft_6() {
        assert_perft(POSITION_PERFT_6, 5, 164_075_551);
    }
}
