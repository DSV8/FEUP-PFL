:- use_module(library(lists)).
:- use_module(library(between)).
:- use_module(library(system), [now/1]).
:- consult(utils).
:- consult(data).

% choose_difficulty(+Bot)
% Choose Bot difficulty (1 or 2)
choose_difficulty(Bot) :-
    format('Please select ~a status:\n', [Bot]),
    write('1 - Random\n'),
    write('2 - Greedy\n'),
    get_option(1, 2, 'Difficulty', Option), !,
    asserta((difficulty(Bot, Option))).

% menu_option(+N)
% Main menu options. Each represents a game mode.
menu_option(1):-
    write('Human vs. Human\n'),
    get_name(player1),
    get_name(player2).
menu_option(2):-
    write('Human vs. Bot\n'),
    get_name(player1),
    asserta((name_of(player2, 'Bot'))), !, 
    choose_difficulty(player2).
menu_option(3):-
    write('Bot vs. Bot\n'),
    asserta((name_of(player1, 'Bot1'))),
    asserta((name_of(player2, 'Bot2'))), !,
    choose_difficulty(player1),
    choose_difficulty(player2).

% choose_player(-Player)
% Unifies player with the player who will start the game
choose_player(Player):-
    name_of(player1, Name1),
    name_of(player2, Name2),
    format('Who starts playing?\n1 - ~a with WHITE pieces\n2 - ~a with BLACK pieces\n', [Name1, Name2]),
    get_option(1, 2, 'Select', Index),
    nth1(Index, [player1, player2], Player).

% game_header/0
% Game header display
game_header:-
    write('+---------------------------+\n'),
    write('|          DIFFERO          |\n'),
    write('+---------------------------+\n').

% menu/0
% Main menu
menu:-
    write('+---------------------------+\n'),
    write('| Please select game mode:  |\n'),
    write('+---------------------------+\n'),
    write('| 1 - Human vs Human        |\n'),
    write('| 2 - Human vs Bot          |\n'),
    write('| 3 - Bot vs Bot            |\n'),
    write('+---------------------------+\n').

% set_mode/0
% Game mode choice
set_mode :-
    menu,
    get_option(1, 3, 'Mode', Option), !,
    menu_option(Option).

% configuration(-GameState)
% Initialize GameState with Board and first Player
configurations([Board,Player,[],0]):-
    game_header,
    set_mode,
    choose_player(Player), 
    init_state(Size, Board).