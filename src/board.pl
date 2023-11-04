:- use_module(library(lists)).
:- consult(data).
:- consult(utils).

% put_piece(+Board, +Coordinate, +Piece, -NewBoard).
% Unifies NewBoard with a matrix representing the placement of Piece on Board in Col-Row coordinates
put_piece(Board, Col-Row, Piece, NewBoard) :-
    RowIndex is Row - 1, ColIndex is Col - 1,
    nth0(RowIndex,Board,Line),
    replace(ColIndex, Piece, Line, NewLine),
    replace(RowIndex, NewLine, Board, NewBoard).

% adjust_position(+InitialCoordinates, -AdjustedCoordinates),
% Adjusts the position to the matrix indexes
adjust_position(Row-Col, Row-NewCol) :-
    (Row =< 5 ->
        RowAux is Row + 1,
        Offset is 5 - RowAux,
        NewCol is Col + Offset
    ;
        NewCol is Col
    ).

% position(+Board, +Coordinate, -Piece)
% Gives the piece on that coordinate in the board
position(Board, Col-Row, Piece) :-
    nth0(Row, Board, Line),
    nth0(Col, Line, Piece).

% valid_position(?Row-Col)
% Checks if the position is valid within our matrix
valid_position(Row-Col) :- 
    adjust_position(Row-Col, Row-NewCol),
    between(0, 4, Row), R is Row + 4 - 2 * Row, between(R, 8, NewCol), !.
valid_position(Row-Col) :- 
    between(5, 8, Row), R is 12 - Row, between(0, R, Col), !.

% initial_state(+Size, -GameState)
% Unifies Board with a Size matrix that represents the game:
initial_state(Size, [Board,_,_]):-
    board(Size, Board).