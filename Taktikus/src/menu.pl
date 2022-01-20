:- include('game.pl').

% dificulty(+Code, -Difficulty) returns the difficulty of the game associated with a code.
dificulty(1, 'Easy').
dificulty(2, 'Normal').

% logo/0, prints the game logo.
logo:-
    write('  ########   ###     ##    ##   ##  ######## ##     ##    ######   \n'),
    write('     ##     ## ##    ##   ##    ##     ##    ##     ##   ##    ##   \n'),
    write('     ##    ##   ##   ##  ##     ##     ##    ##     ##   ##         \n'),
    write('     ##   ##     ##  #####      ##     ##    ##     ##    #####     \n'),
    write('     ##   #########  ##  ##     ##     ##    ##     ##        ##    \n'),
    write('     ##   ##     ##  ##   ##    ##     ##    ##     ##  ##    ##    \n'),
    write('     ##   ##     ##  ##    ##   ##     ##     #######    ######     \n').

% header(+Header) prints the header of a menu using format.
header(Header):-
  format('~n~`*t ~p ~`*t~30|~n', [Header]).

% option(+Option, +Details) prints the selected option menu-like with aditional details using format
option(Option, Details):-
    format('*~t~d~t~5|~t~a~t~20+~t*~30|~n',[Option, Details]).

% menu/0 presents a user friendly menu for game options.
menu:-
    logo,
    header('MENU'),
    option(1, 'Player x Player'),
    option(2, 'Intructions'),
    option(0, 'EXIT'),
    header('*'),
    read_number(0,2,Number),
    menu_option(Number).

% menu_option(+Option)
% Sub-Menus related to option selected on the main menu
% Exit Main Menu
menu_option(0):-
    logo,
    fail.
% Player vs PLayer, need to choose Board Size
menu_option(1):-
    pp_menu(1).
% Player vs Computer, need to choose Board Size

% Choose to exit game on size screen
pp_menu(0):-
  menu.
% Hidden Feature

% Choose Size, Starting Game
pp_menu(1):-
    header('Type a Board Size'),
    option(0, 'EXIT'),
    read_number(0,2,Number),
    Number > 2,
    initial_state(X, GameState),
    game_loop(GameState).
