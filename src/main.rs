use lazy_static::lazy_static;
use std::collections::HashMap;
use std::fmt;
use std::io::Write;

#[derive(PartialEq)]
enum Turn {
    Black,
    White,
}
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
enum Color {
    White,
    Black,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
enum PieceType {
    King,
    Queen,
    Rook,
    Bishop,
    Knight,
    Pawn,
}

#[derive(Clone, Copy)]
struct LastMove {
    from: (usize, usize),
    to: (usize, usize),
    piece: Piece,
}

impl Turn {
    fn toggle(self) -> Turn {
        match self {
            Turn::White => Turn::Black,
            Turn::Black => Turn::White,
        }
    }
}
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
struct Piece {
    _type: PieceType,
    color: Color,
}

impl Piece {
    fn is_white(&self) -> bool {
        self.color == Color::White
    }

    fn is_pawn(&self) -> bool {
        self._type == PieceType::Pawn
    }
}

lazy_static! {
    static ref MOVES: HashMap<PieceType, Vec<(i8, i8)>> = {
        let mut m = HashMap::new();
        m.insert(
            PieceType::King,
            vec![
                (-1, -1),
                (-1, 0),
                (-1, 1),
                (0, -1),
                (0, 1),
                (1, -1),
                (1, 0),
                (1, 1),
            ],
        );
        m.insert(
            PieceType::Knight,
            vec![
                (-2, -1),
                (-2, 1),
                (-1, -2),
                (-1, 2),
                (1, -2),
                (1, 2),
                (2, -1),
                (2, 1),
            ],
        );
        m
    };
}

fn generate_sliding_moves(
    board: &[[Option<Piece>; 8]; 8],
    piece: Piece,
    start: (usize, usize),
    directions: &[(i8, i8)],
) -> Vec<(usize, usize)> {
    let mut result = Vec::new();
    let (start_row, start_col) = (start.0 as i8, start.1 as i8);

    for &(dr, dc) in directions {
        let mut r = start_row + dr;
        let mut c = start_col + dc;

        while (0..8).contains(&r) && (0..8).contains(&c) {
            let r_usize = r as usize;
            let c_usize = c as usize;

            match board[r_usize][c_usize] {
                None => result.push((r_usize, c_usize)), // Empty square
                Some(target_piece) => {
                    if piece.color != target_piece.color {
                        result.push((r_usize, c_usize)); // Capture enemy piece
                    }
                    break; // Stop at first occupied square
                }
            }

            r += dr;
            c += dc;
        }
    }

    result
}

fn generate_pawn_moves(
    board: &[[Option<Piece>; 8]; 8],
    piece: Piece,
    start: (usize, usize),
    viable_en_passant: bool,
) -> Vec<(usize, usize)> {
    let mut moves = Vec::new();
    let (row, col) = start;
    let our_color = piece.color;

    let (dir, start_row) = match (our_color, piece._type) {
        (Color::White, PieceType::Pawn) => (-1, 6),
        (Color::Black, PieceType::Pawn) => (1, 1),
        _ => return moves,
    };

    let new_row = row as i8 + dir;
    if (0..8).contains(&new_row) {
        let new_row_usize = new_row as usize;

        // Forward 1
        if board[new_row_usize][col].is_none() {
            moves.push((new_row_usize, col));

            // Forward 2 from starting row
            if row == start_row {
                let two_row = row as i8 + 2 * dir;
                if (0..8).contains(&two_row) && board[two_row as usize][col].is_none() {
                    moves.push((two_row as usize, col));
                }
            }
        }

        // Diagonal captures
        for dc in [-1, 1] {
            let new_col = col as i8 + dc;
            if (0..8).contains(&new_col) {
                let new_col_usize = new_col as usize;
                match board[new_row_usize][new_col_usize] {
                    Some(target_piece) if target_piece.color != our_color => {
                        moves.push((new_row_usize, new_col_usize));
                    }
                    _ => {}
                }
            }
        }
    }

    // En passant (only valid on the 5th rank for white, 4th for black)
    let en_passant_row = match our_color {
        Color::White => 3,
        Color::Black => 4,
    };

    if viable_en_passant && row == en_passant_row {
        for dc in [-1, 1] {
            let new_col = col as i8 + dc;
            if (0..8).contains(&new_col) {
                let adj_col = new_col as usize;
                match board[row][adj_col] {
                    Some(adj_piece)
                        if adj_piece._type == PieceType::Pawn && adj_piece.color != our_color =>
                    {
                        moves.push(((row as i8 + dir) as usize, adj_col));
                    }
                    _ => {}
                }
            }
        }
    }

    moves
}

fn get_piece_moves(
    board: &[[Option<Piece>; 8]; 8],
    attacks_board: &[[Attacked; 8]; 8],
    piece: Piece,
    pos: (usize, usize),
    viable_en_passant: bool,
) -> Vec<(usize, usize)> {
    match piece._type {
        PieceType::Pawn => generate_pawn_moves(board, piece, pos, viable_en_passant),

        PieceType::Rook => {
            generate_sliding_moves(board, piece, pos, &[(1, 0), (-1, 0), (0, 1), (0, -1)])
        }
        PieceType::Bishop => {
            generate_sliding_moves(board, piece, pos, &[(1, 1), (-1, -1), (1, -1), (-1, 1)])
        }
        PieceType::Queen => generate_sliding_moves(
            board,
            piece,
            pos,
            &[
                (1, 0),
                (-1, 0),
                (0, 1),
                (0, -1),
                (1, 1),
                (-1, -1),
                (1, -1),
                (-1, 1),
            ],
        ),
        _ => MOVES.get(&piece._type).map_or(vec![], |deltas| {
            deltas
                .iter()
                .filter_map(|(delta_row, delta_column)| {
                    let row = pos.0 as i8 + delta_row;
                    let column = pos.1 as i8 + delta_column;
                    if (0..8).contains(&row) && (0..8).contains(&column) {
                        let target = board[row as usize][column as usize];
                        let target_attacked = attacks_board[row as usize][column as usize];
                        let piece_is_white = piece.is_white();
                        let target_is_white = target.is_some_and(|p| p.is_white());
                        let is_safe = !matches!(
                            (piece_is_white, target_attacked),
                            (false, Attacked::ByWhite | Attacked::ByBoth)
                                | (true, Attacked::ByBlack | Attacked::ByBoth)
                        );
                        if (target.is_none() || target_is_white != piece_is_white) && is_safe {
                            Some((row as usize, column as usize))
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                })
                .collect()
        }),
    }
}
impl fmt::Display for Piece {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let symbol = match (self._type, self.color) {
            (PieceType::King, Color::White) => "♔",
            (PieceType::Queen, Color::White) => "♕",
            (PieceType::Rook, Color::White) => "♖",
            (PieceType::Bishop, Color::White) => "♗",
            (PieceType::Knight, Color::White) => "♘",
            (PieceType::Pawn, Color::White) => "♙",
            (PieceType::King, Color::Black) => "♚",
            (PieceType::Queen, Color::Black) => "♛",
            (PieceType::Rook, Color::Black) => "♜",
            (PieceType::Bishop, Color::Black) => "♝",
            (PieceType::Knight, Color::Black) => "♞",
            (PieceType::Pawn, Color::Black) => "♟︎",
        };
        write!(f, "{}", symbol)
    }
}

fn from_letters_to_numbers(input: &str) -> Option<(usize, usize)> {
    if input.len() != 2 {
        return None;
    }

    let mut chars = input.chars();
    let col_char = chars.next()?.to_ascii_uppercase();
    let row_char = chars.next()?;
    println!("{col_char} {row_char}");
    let col = (col_char as u8).checked_sub(b'@')? as usize;
    let row = (row_char.to_digit(10)? as usize).checked_sub(1)?;
    println!("{col} {row}");
    if !(0..8).contains(&row) || !(0..8).contains(&row) {
        None
    } else {
        Some((7 - row, col - 1)) // flip row to match board indexing
    }
}

// fn get_piece(board: &[[Piece; 8]; 8], row: usize, col: usize) -> Piece {
//     board[row][col]
// }

// fn is_occupied(board: &[[Piece; 8]; 8], row: usize, col: usize) -> bool {
//     board[row][col] != Piece::Empty
// }
fn recalculate_attacked(board: &[[Option<Piece>; 8]; 8], attacks_board: &mut [[Attacked; 8]; 8]) {
    // Reset the attacks board to Safe
    for row in attacks_board.iter_mut() {
        for cell in row.iter_mut() {
            *cell = Attacked::Safe;
        }
    }

    for (row_idx, row) in board.iter().enumerate() {
        for (col_idx, &option_piece) in row.iter().enumerate() {
            let Some(piece) = option_piece else {
                continue;
            };

            let attacks = if piece.is_pawn() {
                let mut targets = Vec::new();
                let attacked_by = if piece.is_white() {
                    // White pawns attack diagonally forward (up)
                    if row_idx > 0 {
                        let r = row_idx - 1;
                        if col_idx > 0 {
                            targets.push((r, col_idx - 1));
                        }
                        if col_idx < 7 {
                            targets.push((r, col_idx + 1));
                        }
                    }
                    Attacked::ByWhite
                } else {
                    // Black pawns attack diagonally forward (down)
                    if row_idx < 7 {
                        let r = row_idx + 1;
                        if col_idx > 0 {
                            targets.push((r, col_idx - 1));
                        }
                        if col_idx < 7 {
                            targets.push((r, col_idx + 1));
                        }
                    }
                    Attacked::ByBlack
                };
                (targets, attacked_by)
            } else {
                let moves = get_piece_moves(board, attacks_board, piece, (row_idx, col_idx), false);
                let attacked_by = if piece.is_white() {
                    Attacked::ByWhite
                } else {
                    Attacked::ByBlack
                };
                (moves, attacked_by)
            };

            for (row, col) in attacks.0 {
                attacks_board[row][col] = match (attacks_board[row][col], attacks.1) {
                    (Attacked::Safe, atk) => atk,
                    (Attacked::ByWhite, Attacked::ByBlack)
                    | (Attacked::ByBlack, Attacked::ByWhite) => Attacked::ByBoth,
                    (existing, _) => existing,
                };
            }
        }
    }
}

fn print_board(
    board: &[[Option<Piece>; 8]; 8],
    selected: Option<(usize, usize)>,
    moves: Option<&Vec<(usize, usize)>>,
) {
    // ANSI escape codes for colors
    let orange_bg = "\x1b[48;5;208m"; // orange
    let red_bg = "\x1b[41m"; // red
    let reset = "\x1b[0m";

    println!();

    #[allow(clippy::needless_range_loop)]
    for row in 0..8 {
        print!("{} ", 8 - row); // Print rank (8 to 1)
        for col in 0..8 {
            let coord = (row, col);
            let cell = board[row][col];
            let symbol = match cell {
                Some(piece) => format!("{}", piece),
                None => "·".to_owned(),
            };

            if Some(coord) == selected {
                print!("{} {} {}", orange_bg, symbol, reset);
            } else if moves.is_some_and(|m| m.contains(&coord)) {
                print!("{} {} {}", red_bg, symbol, reset);
            } else {
                print!(" {} ", symbol);
            }
        }
        println!();
    }

    // Print file labels (A to H)
    print!("  ");
    for c in b'A'..=b'H' {
        print!(" {} ", c as char);
    }
    println!("\n");
}

#[derive(Copy, Clone, Debug)]
enum Attacked {
    Safe,
    ByWhite,
    ByBlack,
    ByBoth,
}

fn main() {
    let mut attacks_board: [[Attacked; 8]; 8] = [
        [Attacked::Safe; 8], // 8
        [Attacked::Safe; 8], // 7
        [Attacked::Safe; 8], // 6
        [Attacked::Safe; 8], // 5
        [Attacked::Safe; 8], // 4
        [Attacked::Safe; 8], // 3
        [Attacked::Safe; 8], // 2
        [Attacked::Safe; 8], // 1
    ];
    let mut last_move: Option<LastMove> = None;
    let mut board: [[Option<Piece>; 8]; 8] = [
        [
            Some(Piece {
                _type: PieceType::Rook,
                color: Color::Black,
            }),
            Some(Piece {
                _type: PieceType::Knight,
                color: Color::Black,
            }),
            Some(Piece {
                _type: PieceType::Bishop,
                color: Color::Black,
            }),
            Some(Piece {
                _type: PieceType::Queen,
                color: Color::Black,
            }),
            Some(Piece {
                _type: PieceType::King,
                color: Color::Black,
            }),
            Some(Piece {
                _type: PieceType::Bishop,
                color: Color::Black,
            }),
            Some(Piece {
                _type: PieceType::Knight,
                color: Color::Black,
            }),
            Some(Piece {
                _type: PieceType::Rook,
                color: Color::Black,
            }),
        ],
        [Some(Piece {
            _type: PieceType::Pawn,
            color: Color::Black,
        }); 8],
        [None; 8],
        [None; 8],
        [None; 8],
        [None; 8],
        [Some(Piece {
            _type: PieceType::Pawn,
            color: Color::White,
        }); 8],
        [
            Some(Piece {
                _type: PieceType::Rook,
                color: Color::White,
            }),
            Some(Piece {
                _type: PieceType::Knight,
                color: Color::White,
            }),
            Some(Piece {
                _type: PieceType::Bishop,
                color: Color::White,
            }),
            Some(Piece {
                _type: PieceType::Queen,
                color: Color::White,
            }),
            Some(Piece {
                _type: PieceType::King,
                color: Color::White,
            }),
            Some(Piece {
                _type: PieceType::Bishop,
                color: Color::White,
            }),
            Some(Piece {
                _type: PieceType::Knight,
                color: Color::White,
            }),
            Some(Piece {
                _type: PieceType::Rook,
                color: Color::White,
            }),
        ],
    ];

    recalculate_attacked(&board, &mut attacks_board);
    let mut turn = Turn::White;
    print_board(&board, None, None);

    loop {
        // Clear screen
        print!("\x1B[2J\x1B[1;1H");
        std::io::stdout().flush().unwrap();

        println!(
            "{} to move.",
            match turn {
                Turn::White => "White",
                Turn::Black => "Black",
            }
        );

        // Input loop for selecting a valid piece
        let (selected_row, selected_col, piece, moves): (usize, usize, Piece, Vec<(usize, usize)>) = loop {
            print_board(&board, None, None);
            print!("Select a piece (e.g., E2) or 'exit': ");
            std::io::stdout().flush().unwrap();

            let mut input = String::new();
            if std::io::stdin().read_line(&mut input).is_ok() {
                let input = input.trim();

                if input.eq_ignore_ascii_case("exit") {
                    return;
                }

                if let Some((row, col)) = from_letters_to_numbers(input) {
                    let cell = board[row][col];
                    let is_white = cell.is_some_and(|p| p.is_white());
                    let Some(piece) = cell else {
                        println!("There's no piece on that square.");
                        continue;
                    };

                    if (turn == Turn::White && !is_white) || (turn == Turn::Black && is_white) {
                        println!("{piece}");
                        println!("That piece belongs to the opponent.");
                        continue;
                    }

                    // add here viable_en_passant variable for get_piece_moves

                    let viable_en_passant = match last_move {
                        Some(LastMove { from, to, piece }) if piece._type == PieceType::Pawn => {
                            // Moved two tiles forward?
                            (from.0 as isize - to.0 as isize).abs() == 2
                                && (to.0 == row) // same row as us
                                && ((to.1 as isize - col as isize).abs() == 1) // adjacent file
                        }
                        _ => false,
                    };

                    let legal_moves = get_piece_moves(
                        &board,
                        &attacks_board,
                        piece,
                        (row, col),
                        viable_en_passant,
                    );
                    if legal_moves.is_empty() {
                        println!("That piece has no legal moves.");
                        continue;
                    }

                    break (row, col, piece, legal_moves);
                } else {
                    println!("Invalid input. Please enter something like E2.");
                }
            }
        };

        // Show the board with selected piece and possible moves
        print!("\x1B[2J\x1B[1;1H");
        print_board(&board, Some((selected_row, selected_col)), Some(&moves));

        println!(
            "Selected: {} at {}{}",
            piece,
            (b'A' + selected_col as u8) as char,
            8 - selected_row
        );

        println!(
            "Possible moves: {:?}",
            moves
                .iter()
                .map(|(r, c)| format!("{}{}", (b'A' + *c as u8) as char, 8 - *r))
                .collect::<Vec<_>>()
        );

        print!("Do you want to move this piece? (y/n): ");
        std::io::stdout().flush().unwrap();

        let mut confirm = String::new();
        std::io::stdin().read_line(&mut confirm).unwrap();

        if confirm.trim().eq_ignore_ascii_case("y") {
            // Input loop for choosing destination
            loop {
                print!("Enter destination square: ");
                std::io::stdout().flush().unwrap();

                let mut dest_input = String::new();
                std::io::stdin().read_line(&mut dest_input).unwrap();

                if let Some((dest_row, dest_col)) = from_letters_to_numbers(dest_input.trim()) {
                    if moves.contains(&(dest_row, dest_col)) {
                        // Check for en passant
                        let is_en_passant = piece._type == PieceType::Pawn
                            && board[dest_row][dest_col].is_none()
                            && selected_col != dest_col;

                        if is_en_passant {
                            // Remove the captured pawn
                            let captured_row = if turn == Turn::White {
                                dest_row + 1
                            } else {
                                dest_row - 1
                            };
                            board[captured_row][dest_col] = None;
                        }

                        // Execute move
                        board[dest_row][dest_col] = Some(piece);
                        board[selected_row][selected_col] = None;

                        last_move = Some(LastMove {
                            from: (selected_row, selected_col),
                            to: (dest_row, dest_col),
                            piece,
                        });

                        recalculate_attacked(&board, &mut attacks_board);
                        break;
                    } else {
                        println!("Invalid destination.");
                    }
                } else {
                    println!("Invalid input.");
                }
            }

            // Switch turn after valid move
            turn = turn.toggle();
        }
    }
}
