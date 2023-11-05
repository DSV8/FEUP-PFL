:- use_module(library(lists)).
:- use_module(library(random)).
:- use_module(library(sets)).
:- consult(configurations).
:- consult(board).
:- consult(utils).

% diagonal_move(+PosOrigin,+PosDestination, -Dir)
% Checks if the move is diagonal
diagonal_move(RowI-ColI, RowF-ColF, Dir) :- %Down-Right
    ColI =:= ColF,                                 
    RowI =\= RowF,
    RowDif is RowF - RowI,
    RowDif > 0,
    Dir is 0, !.
diagonal_move(RowI-ColI, RowF-ColF, Dir) :- %Up-Left
    ColI =:= ColF,                                 
    RowI =\= RowF,
    RowDif is RowF - RowI,
    RowDif < 0,
    Dir is 2, !.
diagonal_move(RowI-ColI, RowF-ColF, Dir) :- %Up-Right
    ColI =\= ColF,                                 
    ColDif is ColF - ColI,
    RowI =\= RowF,
    RowDif is RowF - RowI,
    abs(ColDif) =:= abs(RowDif),
    RowDif < 0,
    Dir is 1, !.
diagonal_move(RowI-ColI, RowF-ColF, Dir) :- %Down-Left
    ColI =\= ColF,                                 
    ColDif is ColF - ColI,
    RowI =\= RowF,
    RowDif is RowF - RowI,
    abs(ColDif) =:= abs(RowDif),
    RowDif > 0,
    Dir is -1, !.

% horizontal_move(+PosOrigin, +PosDestination)
% Checks if the move is horizontal
horizontal_move(RowI-ColI, RowF-ColF, Dir) :-
    RowI =:= RowF,
    ColI =\= ColF,
    Dir is (ColF - ColI).

% is_empty(+Board, +Pos)
% Checks if the position is empty
is_empty(Board, Row-Col):-
    position(Board, Row-Col, Value),
    Value == empty.

% count_pieces_on_line(+Board, +Line, +Piece, -Count)
% Counts the number of pieces of the given type on the given line
count_pieces_on_line(Board, Line, Piece, Count) :-
    nth0(Line, Board, Row),
    count_pieces_on_row(Piece, Row, Count).

% count_pieces_on_row(+Piece, +List, -Count)
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

% get_left_of_diagonal(+Pos, -NewPos)
% Calculates the bottom left of the diagonal of the initial Pos.
get_left_of_diagonal(8-Col, 8-Col).
get_left_of_diagonal(Row-0, Row-0).
get_left_of_diagonal(Row-Col, NewRow-NewCol) :-
    Row > 0,
    Col > 0,
    NewRow1 is Row + 1,
    NewCol1 is Col - 1,
    get_left_of_diagonal(NewRow1-NewCol1, NewRow-NewCol).

% list_diagonal_left(+Board, +Pos, ?List)
% Get list of left diagonal based on Pos
list_diagonal_left(_, Row-_, _) :- Row < 0.
list_diagonal_left(Board, Row-Col, List) :-
    Row >= 0,
    position(Board, Row-Col, Element), 
    NewRow is Row - 1,
    NewCol is Col,
    list_diagonal_left(Board, NewRow-NewCol, Rest),
    List = [Element | Rest].

% list_diagonal_right(+Board, +Pos, ?List)
% Get list of right diagonal based on Pos
list_diagonal_right(_, Row-_, _) :- Row < 0.
list_diagonal_right(_, _-Col, _) :- Col > 8.
list_diagonal_right(Board, Row-Col, List) :-
    Row >= 0,
    Col =< 8,
    position(Board, Row-Col, Element),
    NewRow is Row - 1,
    NewCol is Col + 1,    
    list_diagonal_right(Board, NewRow-NewCol, Rest),
    List = [Element | Rest].

% steps_in_diagonal_left(+Board, +PieceType, +RowI-ColI, -Steps)
% Counts the number of steps the given piece can make in the given diagonal
steps_in_diagonal_left(Board, white, _-Col, Steps):-
    list_diagonal_left(Board, 8-Col, List),
    count_pieces_on_row(white, List, WhiteCount),
    count_pieces_on_row(black, List, BlackCount),
    Steps is WhiteCount - BlackCount.
steps_in_diagonal_left(Board, black, _-Col, Steps):-
    list_diagonal_left(Board, 8-Col, List),
    count_pieces_on_row(white, List, WhiteCount),
    count_pieces_on_row(black, List, BlackCount),
    Steps is BlackCount - WhiteCount.

% steps_in_diagonal_right(+Board, +PieceType, +Row-Col, -Steps)
% Counts the number of steps the given piece can make in the given diagonal
steps_in_diagonal_right(Board, white, Row-Col, Steps):-
    get_left_of_diagonal(Row-Col, NewRow-NewCol),
    list_diagonal_right(Board, NewRow-NewCol, List),
    count_pieces_on_row(white, List, WhiteCount),
    count_pieces_on_row(black, List, BlackCount),
    Steps is (WhiteCount - BlackCount).
steps_in_diagonal_right(Board, black, Row-Col, Steps):-
    get_left_of_diagonal(Row-Col, NewRow-NewCol),
    list_diagonal_right(Board, NewRow-NewCol, List),
    count_pieces_on_row(white, List, WhiteCount),
    count_pieces_on_row(black, List, BlackCount),
    Steps is (BlackCount - WhiteCount).

% noOwnGoal(+RowF, +Piece)
% Checks if the move is not to it's own goal
noOwnGoal(RowF, white) :-
    RowF =\= 8.
noOwnGoal(RowF, black) :-
    RowF =\= 0.

% validate_move(+GameState, ?CoordsOrigin, ?CoordsDestination)
% Checks if the move is valid or not
validate_move([Board,Player,_], Row-Col, NewRow-NewCol):-
    valid_position(Row-Col),
    valid_position(NewRow-NewCol),
    is_empty(Board, NewRow-NewCol),
    position(Board, Row-Col, PieceType),
    player_color(Player, Color),
    PieceType == Color,
    diagonal_move(Row-Col, NewRow-NewCol, Dir),
    Dir =:= 0,
    noOwnGoal(NewRow, Color),
    steps_in_diagonal_left(Board, Color, Row-Col, Steps),
    Steps > 0, 
    RowAux is (Row + Steps),
    NewRow == RowAux, !. %Diagonal Down-Right
validate_move([Board,Player,_], Row-Col, NewRow-NewCol):-
    valid_position(Row-Col),
    valid_position(NewRow-NewCol),
    is_empty(Board, NewRow-NewCol),
    position(Board, Row-Col, PieceType),
    player_color(Player, Color),
    PieceType == Color, 
    diagonal_move(Row-Col, NewRow-NewCol, Dir),
    Dir =:= 2,
    noOwnGoal(NewRow, Color),
    steps_in_diagonal_left(Board, Color, Row-Col, Steps),
    Steps > 0, 
    RowAux is (Row - Steps),
    NewRow == RowAux, !. %Diagonal Up-Left
validate_move([Board,Player,_], Row-Col, NewRow-NewCol):-
    valid_position(Row-Col),
    valid_position(NewRow-NewCol),
    is_empty(Board, NewRow-NewCol),
    position(Board, Row-Col, PieceType),
    player_color(Player, Color),
    PieceType == Color, 
    diagonal_move(Row-Col, NewRow-NewCol, Dir),
    Dir =:= 1,
    noOwnGoal(NewRow, Color),
    steps_in_diagonal_right(Board, Color, Row-Col, Steps),
    Steps > 0, 
    RowAux is (Row - Steps),
    NewRow == RowAux, %Diagonal Up-Right
    ColAux is (Col + Steps),
    NewCol == ColAux, !. 
validate_move([Board,Player,_], Row-Col, NewRow-NewCol):-
    valid_position(Row-Col),
    valid_position(NewRow-NewCol),
    is_empty(Board, NewRow-NewCol),
    position(Board, Row-Col, PieceType),
    player_color(Player, Color),
    PieceType == Color,
    diagonal_move(Row-Col, NewRow-NewCol, Dir),
    Dir =:= -1,
    noOwnGoal(NewRow, Color),
    steps_in_diagonal_right(Board, Color, Row-Col, Steps),
    Steps > 0, 
    RowAux is (Row + Steps),
    NewRow == RowAux, %Diagonal Down-Left
    ColAux is (Col - Steps),
    NewCol == ColAux, !. 
validate_move([Board,Player,_], Row-Col, NewRow-NewCol) :- 
    valid_position(Row-Col),
    valid_position(NewRow-NewCol),
    is_empty(Board, NewRow-NewCol),
    position(Board, Row-Col, PieceType),
    player_color(Player, Color),
    PieceType == Color, 
    horizontal_move(Row-Col, NewRow-NewCol, Dir),
    Dir > 0,
    steps_in_row(Board, Color, Row, Steps),
    Steps > 0, 
    NewRow == Row, 
    ColAux is (Col + Steps),
    NewCol == ColAux, !.
validate_move([Board,Player,_], Row-Col, NewRow-NewCol) :- 
    valid_position(Row-Col),
    valid_position(NewRow-NewCol),
    is_empty(Board, NewRow-NewCol),
    position(Board, Row-Col, PieceType),
    player_color(Player, Color),
    PieceType == Color, %Piece is of the same color as the player
    horizontal_move(Row-Col, NewRow-NewCol, Dir),
    Dir < 0,
    steps_in_row(Board, Color, Row, Steps), 
    Steps > 0, 
    NewRow == Row, 
    ColAux is (Col - Steps),
    NewCol == ColAux, !. %Horizontal Left

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
game_cycle(GameState) :-
    \+game_over(GameState, Winner), !,
    display_game(GameState),
    show_winner(GameState, Winner).
game_cycle(GameState) :-
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
    game_header,
    print_board(Board).

% move(+GameState, +Move, -NewGameState)
% Moves a piece
move(GameState, ColI-RowI-ColF-RowF, NewGameState):-                       
    [Board, Player, TotalMoves] = GameState,
    position(Board, RowI-ColI, Piece),
    put_piece(Board, RowI-ColI, empty, NewBoard1),
    put_piece(NewBoard1, RowF-ColF, Piece, NewBoard),
    change_turn(Player, NewPlayer),
    NewTotalMoves is TotalMoves + 1,
    NewGameState = [NewBoard, NewPlayer, NewTotalMoves].

% valid_moves(+GameState, +Player, -ListOfMoves)
% Gets all the valid moves of the given player
valid_moves(GameState, Player, ListOfMoves):-
    [Board,Player,TotalMoves] = GameState,
    player_color(Player, Color),
    ListOfMoves = [],  % Initialize the list
    findall(RowI-ColI-RowF-ColF, (
        between(0, 8, RowI),
        between(0, 8, ColI),
        position(Board, RowI-ColI, Piece),
        Piece == Color,
        between(0, 8, ColF),
        between(0, 8, RowF),
        ColI \= ColF,  % Ensure ColI and ColF are different
        RowI \= RowF,  % Ensure RowI and RowF are different
        validate_move([Board, Player, TotalMoves], RowI-ColI, RowF-ColF),
        writeln(RowI-ColI)
    ), ListOfMoves).

% choose_move(+GameState, +Player, +Level, -Move)
% Choose move for human player
choose_move([Board,Player,TotalMoves], NewColI-NewRowI-NewColF-NewRowF):-
    \+difficulty(Player, _),                  
    repeat,
    length(Board, Size),
    get_move(Size, ColI-RowI-ColF-RowF),
    NewRowI is RowI - 1,
    NewRowF is RowF - 1,
    NewColI is ColI - 1,
    NewColF is ColF - 1,
    validate_move([Board,Player,TotalMoves], NewRowI-NewColI, NewRowF-NewColF), 
    write('Move Validated\n'),
    !.
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