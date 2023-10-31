:- use_module(library(lists)).
:- use_module(library(random)).
:- consult(configurations).
:- consult(board).

% diagonal_move(+PosOrigin,+PosDestination, -NofMoves)
% Checks if the move is diagonal
diagonal_move(ColI-RowI,ColF-RowF, ColDif-RowDif) :-
    ColI =:= ColF,                                  % diagonal II/IV Quadrant
    ColDif is abs(ColF - ColI),
    RowI =/= RowF,
    RowDif is abs(RowF - RowI).
diagonal_move(ColI-RowI,ColF-RowF, ColDif-RowDif) :-
    ColI =/= ColF,                                  % diagonal I/III Quadrant
    ColDif is abs(ColF - ColI),
    RowI =/= RowF,
    RowDif is abs(RowF - RowI),
    ColDif =:= RowDif.

% horizontal_move(+PosOrigin, +PosDestination, -NOfMoves)
% Checks if the move is horizontal
horizontal_move(ColI-RowI, ColF-RowF, ColDif) :-
    RowI =:= RowF,
    ColI =\= ColF,
    ColDif is abs(ColF - ColI),

% path_empty(+Board, +PosDestination)
% Checks if there the destination position of the move is empty
path_empty(Board, ColF-RowF) :-
    position(Board, ColF-RowF, empty).

% count_pieces_on_line(+Board, +Line, +PieceType, -Count)
% Counts the number of pieces of the given type on the given line
count_pieces_on_line(Board, Line, PieceType, Count) :-
    nth0(Line, Board, Row),
    count_pieces_on_row(PieceType, Row, Count).

% count_pieces(+PieceType, +List, -Count)
% Counts the number of pieces of the given type on the given list
count_pieces_on_row(_, [], 0).
count_pieces_on_row(PieceType, [Piece | Rest], Count) :-
    Piece = PieceType,
    count_pieces_on_row(PieceType, Rest, SubCount),
    Count is SubCount + 1,
count_pieces_on_row(PieceType, [_ | Rest], Count) :-
    count_pieces_on_row(PieceType, Rest, Count).

% position_diagonal(+Board, +PosCoordinates, -DiagonalList)
% Gets the diagonal of the given position
/*position_diagonal(Board, Col-Row, DiagonalList) :-
    length(Board, Size),
    MaxIndex is Size - 1,
    MaxIndex =:= Col,
    MaxIndex =:= Row,
    get_diagonal(Board, Col-Row, DiagonalList).*/

% validate_move(+Board, +CoordsOrigin, +CoordsDestination)
% Checks if the move is valid or not
validate_move(GameState,ColI-RowI,ColF-RowF) :-
    [Board,Player,_] = GameState,
    in_bounds(Board,ColI-RowI), in_bounds(Board,ColF-RowF),
    position(Board, ColI-RowI, PieceI), position(Board, ColF-RowF, PieceF),
    \+(piece_info(PieceI, neutral)), piece_info(PieceF, neutral),
    piece_info(PieceType,Player,PieceI),
    diagonal_move(ColI, ColF, NofMoves),
    path_empty(Board, ColF-RowF).
validate_move(GameState,ColI-RowI,ColF-RowF) :-
    [Board,Player,_] = GameState,
    in_bounds(Board,ColI-RowI), in_bounds(Board,ColF-RowF),
    position(Board, ColI-RowI, PieceI), position(Board, ColF-RowF, PieceF),
    \+(piece_info(PieceI, neutral)), piece_info(PieceF, neutral),
    piece_info(PieceType,Player,PieceI),
    horizontal_move(ColI, ColF, NofMoves),
    %TODO
    path_empty(Board, ColF-RowF).


% show_winner(+GameState, +Winner)
% Prints the winner of the game and number of moves they made
show_winner(Winner):-
    name_of(Winner, Name),
    format('Winner is ~a!\n', [Name]).

% game_cycle(+GameState)
% Loop that keeps the game running
game_cycle(GameState):-
    game_over(GameState, Winner), !,
    display_game(GameState),
    show_winner(GameState).
game_cycle(GameState):-
    display_game(GameState),
    print_turn(GameState),
    choose_move(GameState, Move),
    move(GameState, Move, NewGameState), !,
    game_cycle(NewGameState).

% print_turn(+GameState)
% Prints a message declaring whose turn it is
print_turn([_, Player, _, _]):-
    name_of(Player, Name),
    format('Player ~a, is your turn!\n', [Name]), !.

% display_game(+GameState)
% Prints the board
display_game([Board,_,_,_]) :-
    clear_console,
    length(Board, Size),
    game_header.
    % TODO

% move(+GameState, +Move, -NewGameState)
% Moves a piece
move(GameState, ColI-RowI-ColF-RowF, NewGameState):-                       
    [Board,Player,_,TotalMoves] = GameState,
    position(Board,ColI-RowI,Piece),
    put_piece(Board, ColI-RowI, empty, NewBoard1),
    put_piece(NewBoard1, ColF-RowF, Piece, NewBoard),
    other_player(Player, NewPlayer),
    NewTotalMoves is TotalMoves + 1,
    NewGameState = [NewBoard,NewPlayer,NewForcedMoves,NewTotalMoves].

% valid_moves(+GameState, +Player, -ListOfMoves)
% Gets all the valid moves of the given player
valid_moves(GameState, _, ListOfMoves):-
    findall(ColI-RowI-ColF-RowF, validate_move(GameState,ColI-RowI,ColF-RowF), ListOfMoves),
    \+length(ListOfMoves, 0), !.
valid_moves(GameState, Player, ListOfMoves):-
    [Board,Player,_,TotalMoves] = GameState,
    findall(ColI-RowI-ColF-RowF, validate_move([Board,Player,[],TotalMoves],ColI-RowI,ColF-RowF), ListOfMoves).

% choose_move(+GameState,+Player,+Level,-Move)
% Choose move for human player
choose_move([Board,Player,TotalMoves], ColI-RowI-ColF-RowF):-
    \+difficulty(Player, _),                    
    repeat,
    get_move(Board, ColI-RowI-ColF-RowF),                 
    validate_move([Board,Player,TotalMoves], ColI-RowI, ColF-RowF), !.  
choose_move([Board,Player,TotalMoves], Move):-
    difficulty(Player, Level),                  
    choose_move([Board, Player, TotalMoves], Player, Level, Move), !.  
    % TODO


% play/0
% Starts the game and clears data when it ends 
play :-
    configurations(GameState), !,
    game_cycle(GameState),
    clear_data.