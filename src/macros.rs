/// Constructs a bitboard from a given bit set, arranged to appear in text like a chessboard from
/// White's position.
///
/// # Example
///
/// ```
/// use savant::bitboard;
/// use savant::bitboard::Bitboard;
///
/// let pawns = bitboard! [
///     . . . . . . . .
///     1 1 1 1 1 1 1 1
///     . . . . . . . .
///     . . . . . . . .
///     . . . . . . . .
///     . . . . . . . .
///     1 1 1 1 1 1 1 1
///     . . . . . . . .
/// ];
/// assert_eq!(pawns, Bitboard::new(0x00ff00000000ff00));
/// ```
#[macro_export]
macro_rules! bitboard {
    (@parse $acc:tt 1 $($tail:tt)*) => {
        bitboard!(@parse (($acc << 1) + 1) $($tail)*);
    };
    (@parse $acc:tt $other:tt $($tail:tt)*) => {
        bitboard!(@parse ($acc << 1) $($tail)*);
    };
    (@parse $acc:tt) => {
        $crate::bitboard::Bitboard::new($acc.reverse_bits().swap_bytes())
    };
    ($($tail:tt)*) => {
        bitboard!(@parse 0u64 $($tail)*);
    };
}

#[cfg(test)]
mod tests {
    use crate::bitboard::Bitboard;

    #[test]
    fn empty_bitboard() {
        let bitboard = bitboard![
            . . . . . . . .
            . . . . . . . .
            . . . . . . . .
            . . . . . . . .
            . . . . . . . .
            . . . . . . . .
            . . . . . . . .
            . . . . . . . .
        ];
        assert_eq!(bitboard, Bitboard::new(0x0000000000000000));
    }

    #[test]
    fn universe_bitboard() {
        let bitboard = bitboard![
            1 1 1 1 1 1 1 1
            1 1 1 1 1 1 1 1
            1 1 1 1 1 1 1 1
            1 1 1 1 1 1 1 1
            1 1 1 1 1 1 1 1
            1 1 1 1 1 1 1 1
            1 1 1 1 1 1 1 1
            1 1 1 1 1 1 1 1
        ];
        assert_eq!(bitboard, Bitboard::new(0xffffffffffffffff));
    }

    #[test]
    fn random_bitboard() {
        let bitboard = bitboard![
            . 1 1 1 . 1 . .
            . . . 1 . 1 1 .
            1 . 1 . . 1 . .
            1 . 1 . . 1 1 .
            1 . 1 . . . . 1
            . 1 . . 1 1 1 1
            1 . . 1 . . 1 1
            1 1 . . 1 1 1 1
        ];
        let expected = Bitboard::new(0x2e68256585f2c9f3);
        assert_eq!(bitboard, expected);
    }
}
