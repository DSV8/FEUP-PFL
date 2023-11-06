# 2023/2024 PFL Project - Differo

For the 2023/2024 Functional and Logical Programming course, we chose to do the Differo board game.

Group: Differo_7

### Contribution

This project was done by:

- Gonçalo de Castilho Serra Alves Martins (up202108707)
- Diogo Silveira Viana (up202108803)

Both students had a 50% collaboration in the project.

## Instalation and Execution

To install and execute the game Differo, firstly, you will need to download PFL_TP1_T03_Differo7.zip and then proceed to unzip it. Secondly, you will need to use Sicstus to consult the main.pl file which is inside the src directory. Finally, the game starts with the predicate play/0:

```
? - play.
```

## Description of the game

Differo is a game for two players. It is played with a hexagonal board and 13 black and white pieces each.

In this game, you win by calculating the difference in strength between your side and the enemy side, and by supporting your own side or blocking the opponent’s side to reach the goal.

While steadily advancing your own piece, you often find yourself unable to stop the opponent’s pieces that is closing in. This is a fun abstract game with simple rules and thrills.

### Components

- 13 white and 13 black pieces each
- Hexagonal board with 5 on a side

### End condition

- You win when one of your piece reach the goal.
- You lose if you cannot move any of your pieces in your turn.

### Preparation

- Place pieces in predetermined positions.
- Choose your own color. White is the first turn.

### Play procedure

- On your turn, you only move one of your pieces according to the following rules:
- The piece moves on a line. The piece may jump over any number of pieces at once, either your own and your opponent’s.
- The number of steps to move the piece is always (number of your pieces) − (number of your opponent’s pieces) on the line to be moved. If this value is less than or equal to 0, the piece cannot move on the line.
- You cannot move the piece off the board or into a place already occupied by another piece.
- You cannot move the piece into the opponent’s goal.

## Game Logic

### Internal Game State Representation:

The GameState argument is essential to every principal predicate of this implementation. It is formed by 3 elements:

- Board, a square matrix of fixed size. It contais white and black atoms (that are the pieces), empty atoms (matrix cells where a piece could be placed), unused atoms (only included by symetrical reasons, those atoms cannot be used as a part of the board itself), wgoal and bgoal atoms (similair to empty atoms, except the player cannot place any of his pieces in the oponnents goal and if he places any of his pieces in his goal, then the game is over);
- Player, contains the atoms player1 and player2, their function being to tell which player will play on the current turn (each player also has a color associated due to the predicate player_color/2, to assure that the player cannot play the oponnent's pieces)
- TotalMoves, accumulator of the total number of moves during the game. Due to this value it is possible to calculate in how many moves a player winned the game.

TBD.

#### Initial Game State

```prolog
GameState([unused, unused, unused, unused, wgoal, wgoal, wgoal, wgoal, wgoal],
          [unused, unused, unused, empty, black, black, black, black, empty],
          [unused, unused, empty, black, empty, black, empty, black, empty],
          [unused, empty, black, black, black, black, black, black, empty],
          [empty, empty, empty, empty, empty, empty, empty, empty, empty],
          [empty, white, white, white, white, white, white, empty, unused],
          [empty, white, empty, white, empty, white, empty, unused, unused],
          [empty, white, white, white, white, empty, unused, unused, unused],
          [bgoal, bgoal, bgoal, bgoal, bgoal, unused, unused, unused, unused]],    % Board
          player1,                                                                 % Player
          0                                                                        % TotalMoves
         )
```

```
+---------------------------------------------------+
|                      DIFFERO                      |
+---------------------------------------------------+

             [w] - [w] - [w] - [w] - [w]             

          [ ] - [B] - [B] - [B] - [B] - [ ]      

       [ ] - [B] - [ ] - [B] - [ ] - [B] - [ ]   

    [ ] - [B] - [B] - [B] - [B] - [B] - [B] - [ ] 

 [ ] - [ ] - [ ] - [ ] - [ ] - [ ] - [ ] - [ ] - [ ]
    
    [ ] - [W] - [W] - [W] - [W] - [W] - [W] - [ ]
    
       [ ] - [W] - [ ] - [W] - [ ] - [W] - [ ]

          [ ] - [W] - [W] - [W] - [W] - [ ]

             [b] - [b] - [b] - [b] - [b]
```

#### Intermidiate Game State

```prolog
GameState([unused, unused, unused, unused, wgoal, wgoal, wgoal, wgoal, wgoal],
          [unused, unused, unused, empty, black, black, black, black, empty],
          [unused, unused, empty, empty, empty, black, empty, black, empty],
          [unused, black, black, black, black, black, black, empty, empty],
          [empty, empty, empty, white, empty, empty, empty, empty, empty],
          [black, empty, white, white, white, white, white, white, unused],
          [empty, white, empty, empty, empty, white, empty, unused, unused],
          [empty, white, white, white, white, empty, unused, unused, unused],
          [bgoal, bgoal, bgoal, bgoal, bgoal, unused, unused, unused, unused]],    % Board
          player1,                                                                 % Player
          4                                                                        % TotalMoves
         )
```

```
+---------------------------------------------------+
|                      DIFFERO                      |
+---------------------------------------------------+

             [w] - [w] - [w] - [w] - [w]             

          [ ] - [B] - [B] - [B] - [B] - [ ]      

       [ ] - [ ] - [ ] - [B] - [ ] - [B] - [ ]   

    [B] - [B] - [B] - [B] - [B] - [B] - [ ] - [ ] 

 [ ] - [ ] - [ ] - [W] - [ ] - [ ] - [ ] - [ ] - [ ]
    
    [B] - [ ] - [W] - [W] - [W] - [W] - [W] - [W]
    
       [ ] - [W] - [ ] - [ ] - [ ] - [W] - [ ]

          [ ] - [W] - [W] - [W] - [W] - [ ]

             [b] - [b] - [b] - [b] - [b]
```

#### Final Game State

TBD.

### Game State Visualization:

Before starting the game, the user(s) is/are able to configure it. The parameters they are able to configure being:

- 1 - Game mode (Human vs Human, Human vs Bot, Bot vs Bot);
- 2 - Player's usernames;
- 3 - Bot's difficulty levels.

This would be an example of a possible iteration:

```
+---------------------------------------------------+
|                      DIFFERO                      |
+---------------------------------------------------+
| Please select game mode:                          |
+---------------------------------------------------+
| 1 - Human vs Human                                |
| 2 - Human vs Bot                                  |
| 3 - Bot vs Bot                                    |
+---------------------------------------------------+
Mode between 1 and 3: 2

Human vs Bot
player1, please type your username: User

+---------------------------------------------------+
| Please select player2 difficulty:                 |
+---------------------------------------------------+
| 1 - Random                                        |
| 2 - Greedy                                        |
+---------------------------------------------------+
Difficulty between 1 and 2: 2
```

The validation of these choices is assured by the generic predicate get_option/4, which is reusable because of the Context variable.

```prolog
% get_option(+Min, +Max, +Context, -Value)
get_option(Min, Max, Context, Value):-
    format('~a between ~d and ~d: ', [Context, Min, Max]),
    repeat,
    read_number(Value),
    between(Min, Max, Value), !.
```

In the second choice, the username of each user is dynamically placed in the fact base (so it could be accessed through any other predicate) through the name_of/2 predicate.

```prolog
% get_username(+Player)
get_username(Player):-
    format('~a, please type your username: ', [Player]),
    read(Name),
    asserta(name_of(Player, Name)).
```

```prolog
% name_of(+Player, -Name)
:- dynamic name_of/2.
```

In the third choice, the difficulty of each bot is dinamically placed in the fact base through teh difficulty/2 predicate.

```prolog
% choose_difficulty(+Bot)
choose_difficulty(Bot) :-
    write('+---------------------------------------------------+\n'),
    format('| Please select ~a difficulty:                 |\n', [Bot]),
    write('+---------------------------------------------------+\n'),
    write('| 1 - Random                                        |\n'),
    write('| 2 - Greedy                                        |\n'),
    write('+---------------------------------------------------+\n'),
    get_option(1, 2, 'Difficulty', Option), !,
    asserta((difficulty(Bot, Option))).
```

```prolog
% difficulty(+Bot, -Difficulty)
:- dynamic difficulty/2.
```

After making all the choices, the board is finally initialized through the predicate initial_state/2. Since the board is hexagonal we decided to make the boarrd size fixed as 9x9 matrix.

```prolog
% configuration(-GameState)
configurations([Board, Player, 0]):-
    game_header,                             % prints the game header
    set_mode,                                % allows the user to choose the mode
    choose_player(Player),                   % asserts player1 plays with white pieces, player2 with black pieces and player1 starts playing
    initial_state(9, [Board, Player, 0]).
```

Once the GameState is initialized, the board is displayed through the predicate print_board/1 (with the help of a few other auwiliar predicates) in each iteration of the game cycle.

```prolog
% print_board(+Board)
print_board(Board) :-
    nl,
    print_board_aux(Board, 0).

% print_board_aux(+Board, +NOfRow)
print_board_aux([], _).
print_board_aux([Row | Rest], N) :-
    (   (N =:= 0; N =:= 8)
    ->  SpaceCount is 13
    ;   (N =:= 1; N =:= 7)
    ->  SpaceCount is 10
    ;   (N =:= 2; N =:= 6)
    ->  SpaceCount is 7
    ;   (N =:= 3; N =:= 5)
    ->  SpaceCount is 4
    ;   SpaceCount is 1
    ),
    NextN is N + 1,
    print_spaces(SpaceCount),
    print_row(Row),
    print_board_aux(Rest, NextN).

% print_row(+Row)
print_row([]) :- nl, nl, !.
print_row([Cell | [unused | _]]) :-
    Cell \= unused,
    print_cell(Cell),
    print_row([]), !.
print_row([unused | Rest]) :-
    print_row(Rest), !.
print_row([Cell | Rest]) :-
    print_cell(Cell),
    display_dash(Rest),
    print_row(Rest), !.

% print_cell(+Piece)
print_cell(Cell) :-
    (   
        Cell \= unused
    ->  symbol(Cell, Symbol),
        write('['),
        write(Symbol),
        write(']')
    ; true % Do nothing
    ).

% print_spaces(+N)
print_spaces(0).
print_spaces(N) :-
    N > 0,
    write(' '),
    NextN is N - 1,
    print_spaces(NextN).

% display_dash(+List)
display_dash([_|_]) :-
    write(' - ').
display_dash([]).
```

There was also the need to create the predicates piece_info/3 and symbol/2 which translate the context of game state without further dependencies.

```prolog
% piece_info(?Type, ?Player, ?Piece)
piece_info(white, player1, white).
piece_info(black, player2, black).
piece_info(empty, neutral).
piece_info(wgoal, neutral).
piece_info(bgoal, neutral).
piece_info(unused, unused).

% symbol(+Piece, -Symbol)
symbol(black, 'B') :- !.
symbol(white, 'W') :- !.
symbol(empty, ' ') :- !.
symbol(bgoal, 'b') :- !.
symbol(wgoal, 'w') :- !.
symbol(unused, '') :- !.
```

### Move Validation and Execution:

The game runs due to the predicate game_cycle/1 function and it only stops if any of the player wins.

```prolog
% game_cycle(+GameState)
game_cycle(GameState) :-
    game_over(GameState, Winner), !,
    show_winner(GameState, Winner).
game_cycle(GameState) :-
    display_game(GameState),
    print_turn(GameState),
    choose_move(GameState, Move),
    move(GameState, Move, NewGameState), !,
    game_cycle(NewGameState).
```

The player then needs to input 2 different coordinates due to the predicate choose_move/2, those coordinates being the origin position of a piece owned by that player and the destination position of that same piece (where the player wants to place that piece). Both those coordinates that the player inputs are evaluated by the predicate validate_move/3 to check if the move is valid or not.

```prolog
% validate_move(+GameState, ?CoordsOrigin, ?CoordsDestination)
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
```

A move is considered to be a valid one, when:

- The piece moves on a line. The piece may jump over any number of pieces at once, either your own and your opponent’s.
- The number of steps to move the piece is always (number of your pieces) − (number of your opponent’s pieces) on the line to be moved. If this value is less than or equal to 0, the piece cannot move on the line.
- You cannot move the piece off the board or into a place already occupied by another piece.
- You cannot move the piece into the opponent’s goal.

After a selection of a valid move has occured, the positions of the pieces change on the board. And because of that, the GameState also needs to be updated. This is all possible due to the predicate move/3.

```prolog
% move(+GameState, +Move, -NewGameState)
move(GameState, ColI-RowI-ColF-RowF, NewGameState):-                       
    [Board, Player, TotalMoves] = GameState,
    position(Board, RowI-ColI, Piece),
    put_piece(Board, RowI-ColI, empty, NewBoard1),
    put_piece(NewBoard1, RowF-ColF, Piece, NewBoard),
    change_turn(Player, NewPlayer),
    NewTotalMoves is TotalMoves + 1,
    NewGameState = [NewBoard, NewPlayer, NewTotalMoves].
```

### List of Valid Moves:

TBD.

### End of Game:

In case any piece of a player reaches the opponent's goal or if any of the players has no valid moves left on their turn, the game ends and a winner is announced.

```prolog
% game_over(+GameState, -Winner)
game_over([Board,_,_], Winner):- % Check if Row 1 or Row 9 has any opposite colored pieces.
    count_pieces_on_line(Board, 1, white, Count1),
    count_pieces_on_line(Board, 9, black, Count2),
    (   Count1 > 0
    ->  Winner is white
    ;   Count2 > 0
    ->  Winner is black
    ).
game_over([_,Player,_], Winner):- % Check if player has any valid moves left to play.
    valid_moves([_,Player,_], Player, ListOfMoves),
    length(ListOfMoves, 0).
```

### Game State Evaluation:

TBD.

### Computer Plays:

For the bots to decide which move to play, we opted to implement 2 different methods: the random and the greedy.
On one hand, the random method, as the name implies, chooses a move randomly from the valid move list.

```prolog
% choose_move(+GameState, +Player, +Level, -Move)
choose_move(GameState, Player, 1, ColI-RowI-ColF-RowF) :-
    valid_moves(GameState, Player, ListOfMoves),
    generate_random_from_list(ListOfMoves, Random),
    nth0(Random, ListOfMoves, RowI-ColI-RowF-ColF).
```

On the other hand, the greedy method ...

TBD.

## Conclusions 

TBD.

## Bibliography
The main references used were:

- Game rules: https://boardgamegeek.com/boardgame/375056/differo
