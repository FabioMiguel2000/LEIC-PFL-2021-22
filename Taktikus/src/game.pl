
:- include('view.pl').
:- include('board.pl').
:- include('utils.pl').

:- dynamic(game_over/1).

game_board_size(8).     % game_board_size(-N), size of the game board, given by N*N, for Taktikus 8x8 is recommended

game_over(false).       % game over flag, change to true when a game over condition is triggered

% main function that begins the game
play:-
    game_board_size(Size),
    initial_state(Size, GameState),
    game_loop(GameState).

% gameloop, ends when game over flag is achieved
game_loop(GameState):-
    game_over(Flag),
    Flag = false,
    display_game(GameState),
    read_user_input(Move),
    %   MISSING FUNCTION HERE! Check if it is a valid move, if so then let it move
    move(GameState, Move, NewGameState),
    game_loop(NewGameState).

% initializes the game state where,
% GameState = [Board, PlayerTurn]
%       -   Board = 2d array representing the board, according with the given Size
%       -   PlayerTurn = in this case white begins
% Size = Size of the game board (Size x Size)
%       -   Size >= 3, but best using 8
initial_state(Size, GameState):-
    Size < 3,
    GameState = error.

initial_state(Size, [GameBoard|PlayerTurn]):-
    Size > 2,
    create_board(Size, GameBoard),
    PlayerTurn = white.

% changes the player turn
change_player_turn(black, white).
change_player_turn(white, black).

% reads from CLI the piece that the user wants to move, the input should be a chess like move 
% (<OldColumn><OldRow><NewColumn><NewRow>, e.g. b1b3, which means peace on B1 moves to B3)
read_user_input(Move):-
    read(Input),    %expecting something like d4d5.
    atom_chars(Input, InputList),
    parse_input(InputList, Move).


% parses the input from user, and convert it to [[Row, Col], [NewRow, NewCol]]
parse_input([Col|[Row|[NewCol|[NewRow]]]], Move):-
    letterNumber(ColNum, Col),
    letterNumber(NewColNum, NewCol),
    letterNumber(RowNum, Row),
    letterNumber(NewRowNum, NewRow),
    Move = [[RowNum,ColNum],[NewRowNum,NewColNum]].


% move(+GameState, +Move, -NewGameState)
%       -   Move = composed of old postion and new postion
%               - Move = [OldPosition|NewPostion]
%               - Position = [RowNum,ColNum], (e.g. [1,5])
move([GameBoard|PlayerTurn], [[Row,Column],[NewRow,NewColumn]], NewGameState):-
    change_board_element(GameBoard, Row, Column, empty, NewGameBoardTemp),
    change_board_element(NewGameBoardTemp, NewRow, NewColumn, PlayerTurn, NewGameBoard),
    %   MISSING FUNCTION HERE! Check if there is a capture
    change_player_turn(PlayerTurn, NewPlayerTurn),
    NewGameState = [NewGameBoard|NewPlayerTurn].



    

% change_board_element([[w,w,w,w,w],[e,e,e,e,e],[e,e,e,e,e],[e,e,e,e,e],[b,b,b,b,b]], 1,3,e,R).