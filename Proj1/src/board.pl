:- use_module(library(lists)).
:- consult(data).
:- consult(utils).

% put_piece(+Board, +Coordinate, +Piece, -NewBoard).
% Unifies NewBoard with a matrix representing the placement of Piece on Board in Col-Row coordinates
put_piece(Board, Row-Col, Piece, NewBoard) :-
    nth0(Row, Board, Line),
    replace(Col, Piece, Line, NewLine),
    replace(Row, NewLine, Board, NewBoard).

% position(+Board, +Coordinate, -Piece)
% Gives the piece on that coordinate in the board
position(Board, Row-Col, Piece) :-
    nth0(Row, Board, Line),
    nth0(Col, Line, Piece).

% valid_position(?Row-Col)
% Checks if the position is valid within our matrix
valid_position(Row-Col) :- 
    between(0, 4, Row), R is Row + 4 - 2 * Row, between(R, 8, Col), !.
valid_position(Row-Col) :- 
    between(5, 8, Row), R is 12 - Row, between(0, R, Col), !.

% initial_state(+Size, -GameState)
% Unifies Board with a Size matrix that represents the game:
initial_state(Size, [Board,_,_]):-
    board(Size, Board).