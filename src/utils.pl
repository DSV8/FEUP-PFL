:- use_module(library(between)).
:- use_module(library(random)).

% clear_buffer/0
% Clears input buffer
clear_buffer:-
    repeat,
    get_char(C),
    C = '\n'.

% clear_console/0
% Clears console
clear_console:- 
    write('\33\[2J').

% get_line(+Acc, -Result)
% Stores in Result an input line up to endline '\n'
get_line(Acc, Result):-
    get_char(Char),
    Char \= '\n',
    append(Acc, [Char], Acc1),
    get_line(Result, Acc1).
get_line(Acc, Result):-
    atom_chars(Result, Acc).

% abs(+Number,-AbsNumber)
% Stores in AbsNumber the absolute value of Number
abs(X,X) :- X >= 0, !.
abs(X,Y) :- Y is -X.

% get_username(+Player)
% Asks player username. Dynamically associate the username to the player.
get_username(Player):-
    format('~a, please type your username? ', [Player]),
    get_line(Name, []),
    asserta(name_of(Player, Name)).

% read_number(-Number)
% Reads input number from console and stores it into the Number variable 
read_number(X):-
    read_number_aux(X,0).
read_number_aux(X,Acc):- 
    get_code(C),
    between(48, 57, C), !,
    Acc1 is 10*Acc + (C - 48),
    read_number_aux(X,Acc1).
read_number_aux(X,X).

% get_option(+Min, +Max, +Context, -Value)
% Checks if the user input is bewtween Min and Max, depending on the Context given and if it is, stores it into the Value variable
get_option(Min, Max, Context, Value):-
    format('~a between ~d and ~d: ', [Context, Min, Max]),
    repeat,
    read_number(Value),
    between(Min, Max, Value), !.

% get_move(+Board, -Coordinate)
% Stores in Coordinate a valid coordinate given by input, within the Board
get_move(Board, Col1-Row1-Col2-Row2):-
    get_option(1, Size, 'Origin row', Row1),
    (   (Row1 >= 1, Row1 =< 5)
    ->  NofCol1 is Row1 + 4 - 2 * Row1
    ;   NofCol1 is 12 - Row1
    ),
    get_option(1, NofCol1, 'Origin column', Col1),
    get_option(1, Size, 'Destination row', Row2),
    (   (Row2 >= 1, Row2 =< 5)
    ->  NofCol2 is Row2 + 4 - 2 * Row2
    ;   NofCol2 is 12 - Row2
    ),
    get_option(1, NofCol2, 'Destination column', Col2).

% list_length(+List, -Length)
% Stores the length of the list in Length
list_length([], 0).
list_length([_ | Tail], Length) :-
    list_length(Tail, TailLength),
    Length is TailLength + 1.

% replace(+Index, +Element, +List, -Result)
% Stores in Result the list resulting from replacing the element at certain Index of List by Element
replace(Index, Element, List, Result) :-
  nth0(Index, List, _, R),
  nth0(Index, Result, Element, R).

% init_random_state/0
% Initialize the random module
init_random_state :-
    now(X),
    setrand(X).

% generate_random_from_list(+List, -RandomNumber)
% Generates a random number between 1 and the length of the list
generate_random_from_list(List, RandomNumber) :-
    length(List, ListLength),
    random_between(1, ListLength, RandomNumber).
