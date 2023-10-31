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

% position(+Board,+Coordinate,-Piece)
% Unites Piece with the piece on the board at those coordinates
position(Board, Col-Row, Piece) :- 
    nth1(Row, Board, Line),
    nth1(Col, Line, Piece), !.
position(Board, Col-Row, Piece) :- 
    nth1(Row, Board, Line),
    nth1(Col, Line, Piece),
    Piece \= empty, Piece \= wgoal, Piece \= bgoal, !.

% in_bounds(+Board,+Coordinate)
% Checks if calculated coordinate is inside Board
in_bounds(Board, Col-Row) :- 
    length(Board, Size),
    between(1, Size, Row),
    columnsInRow(Board, Row),
    between(1, NOfCol, Col).

% get_symbol(+Board,+Row,+Col,-Symbol)
% Unites Symbol with the part symbol in the Col-Line coordinate of Board
get_symbol(Board, Row, Col, Symbol):-
    position(Board,Col-Row,Piece),
    symbol(Piece, Symbol).

% display_pieces(+Board,+Row,+Col,+Size)
% Displays the Board piece in Line-Col coordinates
display_pieces(_, _, Col, Size):- 
    Col > Size, write('\n  '), !.
display_pieces(Board, Row, Col, Size):-
    get_symbol(Board, Row, Col, Symbol),
    format(' ~a |', [Symbol]),
    NextCol is Col + 1,
    display_pieces(Board, Row, NextCol, Size).

% init_state(+Size,-Board)
% Unifies Board with a Size matrix that represents the game:
init_state(Size, Board):-
    print_board.