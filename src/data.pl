% name_of(+Player, -Name)
% Find the Players name
:- dynamic name_of/2.

% difficulty(+Bot, -Difficulty)
% Find the Bot difficulty
:- dynamic difficulty/2.

% board(+Size, +Matrix)
% Board initial structure
board(9, [
[unused, unused, unused, unused, wgoal, wgoal, wgoal, wgoal, wgoal],
[unused, unused, unused, empty, black, black, black, black, empty],
[unused, unused, empty, black, empty, black, empty, black, empty],
[unused, empty, black, black, black, black, black, black, empty],
[empty, empty, empty, empty, empty, empty, empty, empty, empty],
[empty, white, white, white, white, white, white, empty, unused],
[empty, white, empty, white, empty, white, empty, unused, unused],
[empty, white, white, white, white, empty, unused, unused, unused],
[bgoal, bgoal, bgoal, bgoal, bgoal, unused, unused, unused, unused]
]).

% piece_info(?Type, ?Player, ?Piece)
% It allows to generalize the type of piece and to know the player that uses it
piece_info(white, player1, white).
piece_info(black, player2, black).
piece_info(empty, neutral).
piece_info(wgoal, neutral).
piece_info(bgoal, neutral).
piece_info(unused, unused).

% change_turn(+CurrentPlayer, -NextPlayer)
% Change player turn
change_turn(player1, player2).
change_turn(player2, player1).

% symbol(+Piece, -Symbol)
% Translates the piece to a visible symbol on the board
symbol(black, 'B') :- !.
symbol(white, 'W') :- !.
symbol(empty, ' ') :- !.
symbol(bgoal, 'b') :- !.
symbol(wgoal, 'w') :- !.
symbol(unused, '') :- !.

% print_cell(+Piece)
% Predicate to print a single cell
print_cell(Cell) :-
    (   
        Cell \= unused
    ->  symbol(Cell, Symbol),
        write('['),
        write(Symbol),
        write(']')
    ; true % Do nothing
    ).

% display_dash(+List)
% Predicate to print a dash if there are still members in the list.
display_dash([_|_]) :-
    write(' — ').
display_dash([]).

% print_row(+Row)
% Predicate to print a row of cells
print_row([]) :- nl, nl.
print_row([Cell | Rest]) :-
    print_cell(Cell),
    display_dash(Rest),
    print_row(Rest).

% print_board/0
% Predicate to print the entire hexagonal board
print_board :-
    board(9, Board),
    print_board_aux(Board, 0).

% print_board_aux(+Board, +NOfRow)
% Predicate to print the entire hexagonal board
print_board_aux([], _).
print_board_aux([Row | Rest], N) :-
    (   (N =:= 0; N =:= 8)
    ->  SpaceCount is 4
    ;   (N =:= 1; N =:= 7)
    ->  SpaceCount is 3
    ;   (N =:= 2; N =:= 6)
    ->  SpaceCount is 2
    ;   (N =:= 3; N =:= 5)
    ->  SpaceCount is 1
    ;   SpaceCount is 0
    ),
    NextN is N + 1,
    print_spaces(SpaceCount),
    print_row(Row),
    print_board(Rest, NextN).

% print_spaces(+N)
% Predicate to print spaces before a row of cells
print_spaces(0).
print_spaces(N) :-
    N > 0,
    write(' '),
    NextN is N - 1,
    print_spaces(NextN).

/**
 *
 *  Board structure
 *
 *  I --------------     [w] — [w] — [w] — [w] — [w]             
 *                                                              
 *  H ------------    [ ] — [B] — [B] — [B] — [B] — [ ]      
 *                                                            
 *  G ----------   [ ] — [B] — [ ] — [B] — [ ] — [B] — [ ]   
 *                                                              
 *  F --------  [ ] — [B] — [B] — [B] — [B] — [B] — [B] — [ ] 
 *                                                              
 *  E ------ [ ] — [ ] — [ ] — [ ] — [ ] — [ ] — [ ] — [ ] — [ ]
 *                                                                
 *  D --------  [ ] — [W] — [W] — [W] — [W] — [W] — [W] — [ ]     \
 *                                                                 \
 *  C ----------   [ ] — [W] — [ ] — [W] — [ ] — [W] — [ ]     \    \
 *                                                              \    \ 
 *  B ------------    [ ] — [W] — [W] — [W] — [W] — [ ]    \     \    \
 *                                                          \     \    \
 *  A --------------     [b] — [b] — [b] — [b] — [b]    \    \     \    \
 *                                                       \    \     \    \
 *                          \     \     \     \     \     \    \     \    \
 *                           \     \     \     \     \     \    \     \    \
 *                            1     2     3     4     5     6    7     8    9                  
**/