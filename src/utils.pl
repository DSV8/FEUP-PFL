:- use_module(library(between)).
:- use_module(library(random)).
:- use_module(library(lists)).
:- consult(data).

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

% clear_data/0
% Clears all assertions for the next game
clear_data:-
    retractall(difficulty(_, _)),
    retractall(name_of(_, _)).

% abs(+Number,-AbsNumber)
% Stores in AbsNumber the absolute value of Number
abs(X,X) :- X >= 0, !.
abs(X,Y) :- Y is -X.

% get_username(+Player)
% Asks player username. Dynamically associate the username to the player.
get_username(Player):-
    format('~a, please type your username. ', [Player]),
    read(Name),
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

% get_move(+Size, -Coordinate)
% Stores in Coordinate a valid coordinate given by input, within the Board
get_move(Size, Col1-Row1-Col2-Row2):-
    Max is Size - 1,
    get_option(2, Max, 'Origin row', Row1),
    RowAux1 is Row1 - 1,
    (   (RowAux1 >= 1, RowAux1 =< 4)
    ->  (NofCol1 is RowAux1 + 4 - 2 * RowAux1,
        NofColAux1 is NofCol1 + 1,
        get_option(NofColAux1, Size, 'Origin column', Col1))  
    ;   (NofCol1 is 12 - RowAux1,
        NofColAux1 is NofCol1 + 1,
        get_option(1, NofColAux1, 'Origin column', Col1))
    ),
    get_option(1, Size, 'Destination row', Row2),
    RowAux2 is Row2 - 1,
    (   (RowAux2 >= 0, RowAux2 =< 4)
    ->  (NofCol2 is RowAux2 + 4 - 2 * RowAux2,
        NofColAux2 is NofCol2 + 1,
        get_option(NofColAux2, Size, 'Destination column', Col2))
    ;   (NofCol2 is 12 - RowAux2,
        NofColAux2 is NofCol2 + 1,
        get_option(1, NofColAux2, 'Destination column', Col2))
    ).

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
    getrand(X),
    setrand(X).

% generate_random_from_list(+List, -RandomNumber)
% Generates a random number between 1 and the length of the list
generate_random_from_list(List, RandomNumber) :-
    length(List, ListLength),
    random(1, ListLength, RandomNumber).
