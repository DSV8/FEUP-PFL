:- use_module(library(between)).

% clear_console/0
% Clears console
clear_console:- 
    write('\33\[2J').

% get_line(-Result,+Acc)
% Stores in Result an input line up to endline '\n'
get_line(Result, Acc):-
    get_char(Char),
    Char \= '\n',
    append(Acc, [Char], Acc1),
    get_line(Result, Acc1).
get_line(Result, Acc):-
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

% get_option(+Min,+Max,+Context,-Value)
% Checks if the user input is bewtween Min and Max, depending on the Context given and if it is, stores it into the Value variable
get_option(Min,Max,Context,Value):-
    format('~a between ~d and ~d: ', [Context, Min, Max]),
    repeat,
    read_number(Value),
    between(Min, Max, Value), !.

% get_move(+Board,-Coordinate)
% Stores in Coordinate a valid coordinate given by input, within the Board
get_move(Board, Col1-Row1-Col2-Row2):-
    length(Board, Size),
    get_option(1, Size, 'Origin column', Col1),
    get_option(1, Size, 'Origin row', Row1),
    get_option(1, Size, 'Destination column', Col2),
    get_option(1, Size, 'Destination row', Row2).

% replace(+Index,+Element,+List,-Result)
% Stores in Result the list resulting from replacing the element at certain Index of List by Element
replace(Index, Element, List, Result) :-
  nth0(Index, List, _, R),
  nth0(Index, Result, Element, R).