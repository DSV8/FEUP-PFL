:- use_module(library(lists)).
:- use_module(library(random)).
:- consult(configurations).
:- consult(board).

% diagonal_move(+PosOrigin,+PosDestination)
% Checks if the move is diagonal
diagonal_move(ColI-RowI,ColF-RowF) :-
    ColDif is ColF-ColI,
    RowDif is RowF-RowI,
    abs(ColDif,ColAbsDif),
    abs(RowDif,RowAbsDif).

% horizontal_move(+PosOrigin,+PosDestination)
% Checks if the move is horizontal
horizontal_move(ColI,ColF) :-
    ColDif is ColF-ColI,
    abs(ColDif, AbsDif).

% valid_direction(+PieceType,+PosOrigin,+PosDestination)
% Checks if the direction of the move is valid
valid_direction(Black,ColI-RowI,ColF-RowF) :-
    diagonal_move(ColI-RowI,ColF-RowF).
valid_direction(White,ColI-RowI,ColF-RowF) :-
    diagonal_move(ColI-RowI,ColF-RowF).
valid_direction(Black,ColI,ColF) :-
    horizontal_move(ColI,ColF).
valid_direction(White,ColI,ColF) :-
    horizontal_move(ColI,ColF).

% path_obstructed(+Board,+PosOrigin,+PosDestination)
% Checks if there is a piece in the destination position of the move ---> (WHAT IT SHOULD DO)
% Checks if there is a piece between the two positions of the move ---> (WHAT IT DOES)
path_obstructed(Board, ColI-RowI,ColF-RowF) :-
    DeltaCol is ColF-ColI, DeltaRow is RowF-RowI,
    move_direction(DeltaCol-DeltaRow,HorDir,VerDir),
    \+path_obstructedAux(Board, ColI-RowI,ColF-RowF,HorDir-VerDir).
    % TODO

% path_obstructedAux(+Board,+PosOrigin,+PosDestination,+Direction)
% Auxiliary function of path_obstructed.
path_obstructedAux(_,Col-Row,Col-Row,_) :- !.
path_obstructedAux(Board,ColI-RowI,ColF-RowF,HorDir-VerDir) :-
    NewCol is ColI + HorDir, NewRow is RowI + VerDir,
    position(Board,NewCol-NewRow,Piece),
    piece_info(Piece, neutral), !,
    path_obstructedAux(Board,NewCol-NewRow,ColF-RowF,HorDir-VerDir).
    % TODO

% move_direction(+MoveVector,-HorDir,-VerDir) :-
% Given a move, gives the horizontal and vertical direction with both forming a unit vector
move_direction(DeltaCol-0,-1, 0) :-         %Left Move
    DeltaCol < 0, !.
move_direction(DeltaCol-0,1, 0) :-          %Right Move
    DeltaCol > 0, !.
move_direction(DeltaCol-DeltaRow,-1,-1) :-  %Up-Left move
    (DeltaCol < 0, DeltaRow < 0), !.
move_direction(0-DeltaRow,0,-1) :-          %Up-Right Move
    DeltaRow < 0, !.
move_direction(0-DeltaRow,0,1):-            %Down-Left Move
    DeltaRow > 0, !.
move_direction(DeltaCol-DeltaRow,1,1) :-    %Down-Right move
    (DeltaCol > 0, DeltaRow > 0), !.

% validate_move(+Board,+CoordsOrigin,+CoordsDestination)
% Checks if the move is valid or not
validate_move(GameState,ColI-RowI,ColF-RowF) :-
    [Board,Player,FearList,_] = GameState,
    in_bounds(Board,ColI-RowI), in_bounds(Board,ColF-RowF),
    position(Board, ColI-RowI,PieceI), position(Board, ColF-RowF, PieceF),
    \+(piece_info(PieceI, neutral)), piece_info(PieceF, neutral),
    piece_info(PieceType,Player,PieceI),
    valid_direction(PieceType,ColI-RowI,ColF-RowF),
    \+path_obstructed(Board,ColI-RowI,ColF-RowF).
    % TODO

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
    findall(ColI-RowI-ColF-RowF, validate_move(GameState,ColI-RowI,ColF-RowF),ListOfMoves),
    \+length(ListOfMoves, 0), !.
valid_moves(GameState, Player, ListOfMoves):-
    [Board,Player,_,TotalMoves] = GameState,
    findall(ColI-RowI-ColF-RowF, validate_move([Board,Player,[],TotalMoves],ColI-RowI,ColF-RowF),ListOfMoves).

% choose_move(+GameState,+Player,+Level,-Move)
% Choose move a human player
choose_move([Board,Player,ForcedMoves,TotalMoves], ColI-RowI-ColF-RowF):-
    \+difficulty(Player, _),                    
    repeat,
    get_move(Board, ColI-RowI-ColF-RowF),                 
    validate_move([Board,Player,ForcedMoves,TotalMoves], ColI-RowI, ColF-RowF), !.  
choose_move([Board,Player,ForcedMoves,TotalMoves], Move):-
    difficulty(Player, Level),                  
    choose_move([Board,Player,ForcedMoves,TotalMoves], Player, Level, Move), !.  
    % TODO


% play/0
% Starts the game and clears data when it ends 
play :-
    configurations(GameState), !,
    game_cycle(GameState),
    clear_data.