:- use_module(library(lists)).
:- use_module(library(random)).
:- use_module(library(sets)).
:- consult(configurations).
:- consult(board).
:- consult(utils).

% diagonal_move(+PosOrigin,+PosDestination, -NofMoves)
% Checks if the move is diagonal
diagonal_move(ColI-RowI, ColF-RowF, RowDif) :-
    ColI =:= ColF,                                  % diagonal II/IV Quadrant
    RowI =\= RowF,
    RowDif is abs(RowF - RowI).
diagonal_move(ColI-RowI, ColF-RowF, RowDif) :-
    ColI =\= ColF,                                  % diagonal I/III Quadrant
    ColDif is abs(ColF - ColI),
    RowI =\= RowF,
    RowDif is abs(RowF - RowI),
    ColDif =:= RowDif.

% horizontal_move(+PosOrigin, +PosDestination, -NOfMoves)
% Checks if the move is horizontal
horizontal_move(ColI-RowI, ColF-RowF, ColDif) :-
    RowI =:= RowF,
    ColI =\= ColF,
    ColDif is abs(ColF - ColI).

% move_direction(+MoveVector, -Dir)
% Given a move, gives the horizontal and vertical direction with both forming a unit vector
move_direction(DeltaRow-0, 0) :-  %Up-Left move
    DeltaRow < 0, !.
move_direction(DeltaRow-DeltaCol, 1) :- %Up-Right Move
    (DeltaRow < 0, DeltaCol > 0), !.
move_direction(DeltaRow-DeltaCol, 1):-  %Down-Left Move
    (DeltaRow > 0, DeltaCol < 0), !.
move_direction(DeltaRow-0, 0) :-    %Down-Right move
    DeltaRow > 0, !.

% count_pieces_on_line(+Board, +Line, +Piece, -Count)
% Counts the number of pieces of the given type on the given line
count_pieces_on_line(Board, Line, Piece, Count) :-
    nth1(Line, Board, Row),
    count_pieces_on_row(Piece, Row, Count).

% count_pieces(+Piece, +List, -Count)
% Counts the number of pieces of the given type on the given list
count_pieces_on_row(_, [], 0).
count_pieces_on_row(Piece, [Piece | Rest], Count) :-
    count_pieces_on_row(Piece, Rest, SubCount),
    Count is SubCount + 1.
count_pieces_on_row(Piece, [_ | Rest], Count) :-
    count_pieces_on_row(Piece, Rest, Count).

% steps_in_row(+Board, +PieceType, +RowI, -Count)
% Counts the number of steps the given piece can make in the given row
steps_in_row(Board, white, RowI, Count) :-
    count_pieces_on_line(Board, RowI, white, Count1),
    count_pieces_on_line(Board, RowI, black, Count2),
    Count is Count1 - Count2,
    Count > 0.
steps_in_row(Board, black, RowI, Count) :-
    count_pieces_on_line(Board, RowI, black, Count1),
    count_pieces_on_line(Board, RowI, white, Count2),
    Count is Count1 - Count2,
    Count > 0.

% traverse_diagonal(+Board, +Row-Col, +Dir, ?List)
% Helper predicate to traverse the diagonal and collect values.
traverse_diagonal(Board, 1-_, 1, []) :- % We're at the start of the diagonal - DownLeft.
    position(Board, Row-Col, Value),
    Value \= wgoal,
    Value \= bgoal,
    Value \= unused,
    NextRow1 is Row + 1,
    NextCol1 is Col - 1,
    traverse_diagonal(Board, NextRow1-NextCol1, 1, PartialDiagonal1),
    append(PartialDiagonal1, [Value-Row-Col | Diagonal]).
traverse_diagonal(Board, 1-_, 0, []) :- % We're at the start of the diagonal - DownRight.
    position(Board, Row-Col, Value),
    Value \= wgoal,
    Value \= bgoal,
    Value \= unused,
    NextRow1 is Row + 1,
    traverse_diagonal(Board, NextRow1-Col, 0, PartialDiagonal1),
    append(PartialDiagonal1, [Value-Row-Col | Diagonal]).
traverse_diagonal(Board, Row-Col, 0, Diagonal) :- %0 means we're going Up-Left or Down-Right
    position(Board, Row-Col, Value),
    Value \= wgoal,
    Value \= bgoal,
    Value \= unused,
    NextRow1 is Row + 1,
    traverse_diagonal(Board, NextRow1-Col, 0, PartialDiagonal1),
    append(PartialDiagonal1, [Value-Row-Col | Diagonal]).
traverse_diagonal(Board, Row-Col, 1, Diagonal) :- %1 means we're going Up-Right or Down-Left
    position(Board, Row-Col, Value),
    Value \= wgoal,
    Value \= bgoal,
    Value \= unused,
    NextRow1 is Row + 1,
    NextCol1 is Col - 1,
    traverse_diagonal(Board, NextRow1-NextCol1, 1, PartialDiagonal1),
    append(PartialDiagonal1, [Value-Row-Col | Diagonal]).

% diagonal_start(+Row-Col, +Dir, -NewRow-NewCol)
% Given a position, calculates the start of that diagonal
diagonal_start(1-Col, _, NewRow-NewCol):-
    NewRow is 1,
    NewCol is Col.
diagonal_start(Row-8, 1, NewRow-NewCol):-
    NewRow is Row,
    NewCol is 8.
diagonal_start(Row-Col, 0, NewRow-NewCol) :-
    position(Board, Row-Col, Value),
    (Value \= unused ->
        NewRow is Row - 1,
        NewCol is Col,
        diagonal_start(NewRow-NewCol, 0)
    ; % else
        NewRow is Row + 1,
        NewCol is Col
    ).
diagonal_start(Row-Col, 1, NewRow-NewCol) :-
    position(Board, Row-Col, Value),
    (Value \= wgoal ->
        NewRow is Row - 1,
        NewCol is Col + 1,
        diagonal_start(NewRow-NewCol, 1)
    ; % else
        NewRow is Row + 1,
        NewCol is Col - 1
    ).

print_list([]).  % Base case: an empty list, nothing to print.

print_list([X | Rest]) :-
    write(X), nl,  % Write the current element and a newline.
    print_list(Rest).  % Recursively print the rest of the list.

% steps_in_diagonal(+Board, +PieceType, +RowI, +Dir, -Count)
% Counts the number of steps the given piece can make in the given diagonal
steps_in_diagonal(Board, white, RowI-ColI, 0, Count) :-
    diagonal_start(RowI-ColI, 0, NewRowI-NewColI),
    traverse_diagonal(Board, NewRowI-NewColI, 0, List1),
    list_to_set(List1, List),
    count_pieces_on_row(white, List, Count1),
    count_pieces_on_row(black, List, Count2),
    Count is Count1 - Count2,
    Count > 0.
steps_in_diagonal(Board, black, RowI-ColI, 0, Count) :-
    diagonal_start(RowI-ColI, 0, NewRowI-NewColI),
    traverse_diagonal(Board, NewRowI-NewColI, 0, List1),
    list_to_set(List1, List),
    count_pieces_on_row(black, List, Count1),
    count_pieces_on_row(white, List, Count2),
    Count is Count1 - Count2,
    Count > 0.
steps_in_diagonal(Board, white, RowI-ColI, 1, Count) :-
    diagonal_start(RowI-ColI, 0, NewRowI-NewColI),
    traverse_diagonal(Board, NewRowI-NewColI, 1, List1),
    list_to_set(List1, List),
    print_list(List),
    count_pieces_on_row(white, List, Count1),
    count_pieces_on_row(black, List, Count2),
    Count is Count1 - Count2,
    Count > 0.
steps_in_diagonal(Board, black, RowI-ColI, 1, Count) :-
    diagonal_start(RowI-ColI, 0, NewRowI-NewColI),
    traverse_diagonal(Board, NewRowI-NewColI, 1, List1),
    list_to_set(List1, List),
    count_pieces_on_row(black, List, Count1),
    count_pieces_on_row(white, List, Count2),
    Count is Count1 - Count2,
    Count > 0.

% noOwnGoal(+RowF, +Piece)
% Checks if the move is not to it's own goal
noOwnGoal(RowF, white) :-
    RowF =\= 9.
noOwnGoal(RowF, black) :-
    RowF =\= 1.

% validate_move(+Board, ?CoordsOrigin, ?CoordsDestination)
% Checks if the move is valid or not
validate_move(GameState, ColI-RowI, ColF-RowF) :-
    [Board,Player,_] = GameState,
    NewRowI is RowI - 1, 
    NewColI is ColI - 1,
    NewRowF is RowF - 1,
    NewColF is ColF - 1,
    write(NewRowI), write(' '), write(NewColI), write(' '), write(NewRowF), write(' '), write(NewColF), write('\n'),
    valid_position(NewRowI-NewColI), valid_position(NewRowF-NewColF),
    write('valid position\n'),
    position(Board, NewColI-NewRowI, PieceI), position(Board, NewColF-NewRowF, PieceF),
    write(PieceI), write(' '), write(PieceF), write('\n'),
    \+(piece_info(PieceI, neutral)), piece_info(PieceF, neutral),
    player_color(Player, PieceI),
    write(Player), write('\n'),
    diagonal_move(NewColI-NewRowI, NewColF-NewRowF, NofMoves),
    write(NofMoves), write('\n'),
    DeltaRow is (NewRowF - NewRowI),
    DeltaCol is (NewColF - NewColI),
    move_direction(DeltaRow-DeltaCol, Dir),
    write(Dir), write('\n'),
    noOwnGoal(NewRowF, PieceI),
    steps_in_diagonal(Board, PieceI, NewRowI-NewColI, Dir, MoveCount),
    write(MoveCount), write('\n'),
    NofMoves =:= MoveCount.
validate_move(GameState, ColI-RowI, ColF-RowF) :-
    [Board,Player,_] = GameState,
    NewRowI is RowI - 1, 
    NewColI is ColI - 1,
    NewRowF is RowF - 1,
    NewColF is ColF - 1,
    valid_position(NewRowI-NewColI), valid_position(NewRowF-NewColF),
    position(Board, NewColI-NewRowI, PieceI), position(Board, NewColF-NewRowF, PieceF),
    \+(piece_info(PieceI, neutral)), piece_info(PieceF, neutral),
    player_color(Player, PieceI),
    write('before horizontal move\n'),
    horizontal_move(NewColI-NewRowI, NewColF-NewRowF, NofMoves),
    write(NofMoves), write('\n'),
    steps_in_row(Board, PieceI, NewRowI, MoveCount),
    write(MoveCount), write('\n'),
    NofMoves =:= MoveCount.

% winnerMoves(+Moves, -WinnerMoves)
% Gets the number of moves the winner made
winner_moves(Moves, WinnerMoves):-
    Moves mod 2 =:= 1,
    WinnerMoves is (Moves // 2) + 1, !.
winner_moves(Moves, WinnerMoves):-
     WinnerMoves is Moves // 2.

% show_winner(+GameState, +Winner)
% Prints the winner of the game and number of moves they made
show_winner([_,_,_,TotalMoves], Winner):-
    name_of(Winner, Name),
    winner_moves(TotalMoves, WinnerMoves),
    format('Winner is ~a with ~d moves!\n', [Name, WinnerMoves]).

% game_over(+GameState, -Winner)
% Checks if the game is over
game_over([Board, _, _], Winner):- % Check if Row 1 or Row 9 has any opposite colored pieces.
    count_pieces_on_line(Board, 1, white, Count1),
    count_pieces_on_line(Board, 9, black, Count2),
    (   Count1 > 0
    ->  Winner is player1
    ;   Count2 > 0
    ->  Winner is player2
    ).
game_over([Board, Player, _], Winner):- % Check if player has any valid moves left to play.
    valid_moves([Board, Player, _], Player, ListOfMoves),
    length(ListOfMoves, 0).

% game_cycle(+GameState)
% Loop that keeps the game running
%game_cycle(GameState)
    %\+game_over(GameState, Winner), !,
    %display_game(GameState),
    %show_winner(GameState, Winner).
game_cycle(GameState):-
    display_game(GameState),
    print_turn(GameState),
    choose_move(GameState, Move),
    move(GameState, Move, NewGameState), !,
    game_cycle(NewGameState).

% print_turn(+GameState)
% Prints a message declaring whose turn it is
print_turn([_, Player, _]):-
    name_of(Player, Name),
    format('Player ~a, is your turn!\n', [Name]), !.

% display_game(+GameState)
% Prints the board
display_game([Board,_,_]) :-
    %clear_console,
    game_header,
    print_board(Board).

% move(+GameState, +Move, -NewGameState)
% Moves a piece
move(GameState, ColI-RowI-ColF-RowF, NewGameState):-                       
    [Board, Player, TotalMoves] = GameState,
    position(Board,ColI-RowI,Piece),
    put_piece(Board, ColI-RowI, empty, NewBoard1),
    put_piece(NewBoard1, ColF-RowF, Piece, NewBoard),
    change_turn(Player, NewPlayer),
    NewTotalMoves is TotalMoves + 1,
    NewGameState = [NewBoard, NewPlayer, NewTotalMoves].

% valid_moves(+GameState, +Player, -ListOfMoves)
% Gets all the valid moves of the given player
valid_moves(GameState, _, ListOfMoves):-
    findall(ColI-RowI-ColF-RowF, (
        between(1, 9, ColI),
        between(1, 9, RowI),
        between(1, 9, ColF),
        between(1, 9, RowF),
        ColI \= ColF,  % Ensure ColI and ColF are different
        RowI \= RowF,  % Ensure RowI and RowF are different
        validate_move(GameState, ColI-RowI, ColF-RowF)), ListOfMoves),
    \+length(ListOfMoves, 0), !.
valid_moves(GameState, Player, ListOfMoves):-
    [Board,Player,TotalMoves] = GameState,
    findall(ColI-RowI-ColF-RowF, (
        between(1, 9, ColI),
        between(1, 9, RowI),
        between(1, 9, ColF),
        between(1, 9, RowF),
        ColI \= ColF,  % Ensure ColI and ColF are different
        RowI \= RowF,  % Ensure RowI and RowF are different
        validate_move([Board,Player,TotalMoves], ColI-RowI, ColF-RowF)), ListOfMoves).

% choose_move(+GameState, +Player, +Level, -Move)
% Choose move for human player
choose_move([Board,Player,TotalMoves], ColI-RowI-ColF-RowF):-
    \+difficulty(Player, _),                  
    repeat,
    length(Board, Size),
    get_move(Size, ColI-RowI-ColF-RowF),
    validate_move([Board,Player,TotalMoves], ColI-RowI, ColF-RowF), !,
    write('move validated\n').
choose_move([Board,Player,TotalMoves], ColI-RowI-ColF-RowF):-
    difficulty(Player, Level),                  
    choose_move([Board, Player, TotalMoves], Player, Level, ColI-RowI-ColF-RowF), !.
choose_move(GameState, Player, 1, ColI-RowI-ColF-RowF) :-
    valid_moves(GameState, Player, ListOfMoves),
    generate_random_from_list(ListOfMoves, Random),
    nth0(Random, ListOfMoves, ColI-RowI-ColF-RowF).

% play/0
% Starts the game and clears data when it ends 
play :-
    configurations(GameState), !,
    game_cycle(GameState),
    clear_data.