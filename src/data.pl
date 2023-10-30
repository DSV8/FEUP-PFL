% name_of(+Player, -Name)
% Find the Players name
:- dynamic name_of/2.

% difficulty(+Bot,-Difficulty)
% Find the Bot difficulty
:- dynamic difficulty/2.

% board(+Size,+Matrix)
% Board initial structure
board(9, [
                    [bgoal, bgoal, bgoal, bgoal, bgoal],
                [empty, white, white, white, white, empty],
            [empty, white, empty, white, empty, white, empty],
        [empty, white, white, white, white, white, white, empty],
    [empty, empty, empty, empty, empty, empty, empty, empty, empty],
        [empty, black, black, black, black, black, black, empty],
            [empty, black, empty, black, empty, black, empty],
                [empty, black, black, black, black, empty],
                    [wgoal, wgoal, wgoal, wgoal, wgoal]
]).

% piece_info(?Type,?Player,+Piece)
% It allows to generalize the type of piece and to know the player that uses it
piece_info(white, player1, white).
piece_info(black, player2, black).
piece_info(empty, neutral).
piece_info(wgoal, neutral).
piece_info(bgoal, neutral).

% change_turn(+CurrentPlayer,-NextPlayer)
% Change player turn
change_turn(player1, player2).
change_turn(player2, player1).

% symbol(+Piece,-Symbol)
% Translates the piece to a visible symbol on the board
symbol(black, 'B') :- !.
symbol(white, 'W') :- !.
symbol(empty, ' ') :- !.
symbol(bgoal, 'b') :- !.
symbol(wgoal, 'w') :- !.

/**
 *
 *  Board structure
 *
 *  I --------------   w — w — w — w — w
 *                    / \ / \ / \ / \ / \ 
 *  H ------------     — B — B — B — B —  
 *                  / \ / \ / \ / \ / \ / \ 
 *  G ----------     — B —   — B —   — B —  
 *                / \ / \ / \ / \ / \ / \ / \
 *  F --------     — B — B — B — B — B — B —  
 *              / \ / \ / \ / \ / \ / \ / \ / \
 *  E ------     —   —   —   —   —   —   —   —  
 *              \ / \ / \ / \ / \ / \ / \ / \ /  
 *  D --------     — W — W — W — W — W — W —     \
 *                \ / \ / \ / \ / \ / \ / \ /     \
 *  C ----------     — W —   — W —   — W —     \   \
 *                  \ / \ / \ / \ / \ / \ /     \   \ 
 *  B ------------     — W — W — W — W —     \   \   \
 *                    \ / \ / \ / \ / \ /     \   \   \
 *  A --------------   b — b — b — b — b   \   \   \   \
 *                                          \   \   \   \
 *                       \   \   \   \   \   \   \   \   \
 *                        \   \   \   \   \   \   \   \   \
 *                         1   2   3   4   5   6   7   8   9                  
**/