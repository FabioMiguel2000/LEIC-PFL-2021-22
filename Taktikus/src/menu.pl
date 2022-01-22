# :- include('game.pl').

% dificulty(+Code, -Difficulty) returns the difficulty of the game associated with a code.
dificulty(1, 'Easy').
dificulty(2, 'Normal').

% logo/0, prints the game logo.
logo:-
    write('  ########   ###     ##    ##   ##  ######## ##     ##    #######  \n'),
    write('     ##     ## ##    ##   ##    ##     ##    ##     ##   ##    ##   \n'),
    write('     ##    ##   ##   ##  ##     ##     ##    ##     ##   ##         \n'),
    write('     ##   ##     ##  #####      ##     ##    ##     ##    #####     \n'),
    write('     ##   #########  ##  ##     ##     ##    ##     ##        ##    \n'),
    write('     ##   ##     ##  ##   ##    ##     ##    ##     ##  ##    ##    \n'),
    write('     ##   ##     ##  ##    ##   ##     ##     #######   #######     \n').

% menu_formater(+Info) prints the information of to be used within our menu using format.
menu_formater(Info):-
  format('~n~`*t ~p ~`*t~30|~n', [Info]).

% option(+Option, +Details) prints the selected option menu-like with aditional details using format
option(Option, Details):-
    format('*~t~d~t~5|~t~a~t~20+~t*~30|~n',[Option, Details]).

% menu/0 presents a user friendly menu for game options.
menu:-
    logo,
    menu_formater('MENU'),
    option(1, 'Player x Player'),
    option(2, 'Player x Computer'),
    option(3, 'Intructions'),
    option(0, 'EXIT'),
    menu_formater('*'),
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
    menu_game(1).

% Player vs Computer, need to choose Board Size
menu_option(2):-
    menu_game(2).

% Choose to exit game on size screen
menu_pxp(0):-
  menu.

% Choose Size, Starting Game
menu_pxp(Size):-
    initial_state(Size, GameState),
    game_loop(GameState).

% When facing another player    
menu_game(1):-
    board_selection(Size),
    read_number_board(0,9,Size),
    menu_pxp(Size).

% Selects the size of the board   
board_selection(Size):-
    menu_formater('Type a Board Size'),
    option(3, 'Will produce a 3x3 board'),
    option(8, 'Will produce a 8x8 board'),
    option(0, 'EXIT'),
    read_number_board(0,9,Size).

% When facing the computer
menu_game(2):-
    board_selection(Size),
    menu_pxc(Size).

%menu_pxc(0):-
%    menu_pxp(0)

% When player selecs to play angainst machine
menu_pxc(2):-
    ai_level(Opt), % Yet to be implemented ??
    initial_state(Size, GameState),
    game_loop(GameState).*/

    


