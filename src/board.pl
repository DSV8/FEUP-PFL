:- use_module(library(lists)).
:- consult(data).
:- consult(utils).

% put_piece(+Board,+Coordinate,+Piece,-NewBoard).
% Unifies NewBoard with a matrix representing the placement of Piece on Board in Col-Row coordinates
put_piece(Board, Col-Row, Piece, NewBoard) :-
    RowIndex is Row - 1, ColIndex is Col - 1,
    nth0(RowIndex,Board,Line),
    replace(ColIndex, Piece, Line, NewLine),
    replace(RowIndex, NewLine, Board, NewBoard).

% in_bounds(+Board,+Coordinate)
% Checks if calculated coordinate is inside Board
in_bounds(Board, Col-Row) :-
    length(Board, Size),
    between(1, Size, Col),
    between(1, Size, Row).

% init_state(+Size,-Board)
% Unifies Board with a Size matrix that represents the game: animals and empty pieces
init_state(Size, Board):-
    board(Size, Board).
