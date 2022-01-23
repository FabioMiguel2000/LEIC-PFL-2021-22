:- consult('view.pl').
:- consult('board.pl').
:- consult('moves.pl').
:- consult('capture.pl').
:- consult('utils.pl').
:- consult('menu.pl').


:- use_module(library(between)).
:- use_module(library(random)).

:- dynamic(game_over/1).


:- dynamic(bot/1).
:- dynamic(bot_level/1).


bot(black).
bot_level(1).

game_over(false).       % game over flag, change to true when a game over condition is triggered

% main function that begins the game
play:- menu.
    % bot(Bot),
    % Bot = false,
    % !,
    % menu(Size),
    % initial_state(Size, GameState),
    % game_loop(GameState).

 % with bot player
% play:-
%     game_board_size(Size),
%     initial_state(Size, GameState),
%     computer_player_game(GameState).

% Bot Turn
computer_player_game([GameBoard|PlayerTurn]):-
    bot(Bot),
    bot_level(BotLevel),
    Bot = PlayerTurn,
    !,
    choose_move([GameBoard|PlayerTurn], BotLevel, Move),
    move([GameBoard|PlayerTurn], Move, NewGameState), 
    computer_player_game(NewGameState).

% Player Turn
computer_player_game(GameState):-
    display_game(GameState),
    valid_moves(GameState, ListOfMoves),
    read_user_input(Move, ListOfMoves),
    move(GameState, Move, NewGameState),   
    computer_player_game(NewGameState).

% gameloop, ends when game over flag is achieved
game_loop(GameState):-
    % game_over(Flag),
    % Flag = false,
    % !,
    display_game(GameState),
    valid_moves(GameState, ListOfMoves),
    read_user_input(Move, ListOfMoves),
    move(GameState, Move, NewGameState),   
    game_loop(NewGameState).

% game_loop(_):-
%     write('Game Over!').

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
read_user_input(Move, ListOfMoves):-
    repeat,
    read(Input),    %expecting something like d4d5.
    atom_chars(Input, InputList),
    parse_input(InputList, Move, ListOfMoves).

% checks if the input given by user lays between the board range
valid_input_board_range(Col, Row, NewCol, NewRow):-
    game_board_size(BoardSize),
    ColCodeUpperLimit is 97 + BoardSize -1,
    RowCodeUpperLimit is 49 + BoardSize -1,

    char_code(Col, ColCode),
    between(97, ColCodeUpperLimit, ColCode),         % ascci code of 'a' to gameboard size character

    char_code(Row, RowCode),
    between(1, RowCodeUpperLimit, RowCode),         % ascci code of '1' to gameboard size

    char_code(NewCol, NewColCode),
    between(97, ColCodeUpperLimit, NewColCode),         % ascci code of 'a' to gameboard size character

    char_code(NewRow, NewRowCode),
    between(1, RowCodeUpperLimit, NewRowCode).         % ascci code of '1' to gameboard size

% checks if there is actually a move
different_pos(Col, Row, Col, Row):-
    !,
    fail.

different_pos(_,_,_,_).


% parses and validates the input from user, and convert it to [[Row, Col], [NewRow, NewCol]]
parse_input([Col|[Row|[NewCol|[NewRow]]]], Move, ListOfMoves):-
    valid_input_board_range(Col, Row, NewCol, NewRow),      % checks if it is an input inside the board range
    different_pos(Col, Row, NewCol, NewRow),                % not same position
    numberLetter(ColNum, Col),
    numberLetter(NewColNum, NewCol),
    numberLetter(RowNum, Row),
    numberLetter(NewRowNum, NewRow),
    memberchk([[RowNum,ColNum],[NewRowNum,NewColNum]], ListOfMoves),            % Checks it is a valid move
    !,
    Move = [[RowNum,ColNum],[NewRowNum,NewColNum]].


parse_input(_, _,_):-
    log_invalid_move,
    fail.



% move(+GameState, +Move, -NewGameState)
%       -   Move = composed of old postion and new postion
%               - Move = [OldPosition|NewPostion]
%               - Position = [RowNum,ColNum], (e.g. [1,5])
move([GameBoard|PlayerTurn], [[Row,Column],[NewRow,NewColumn]], NewGameState):-
    change_board_element(GameBoard, Row, Column, empty, NewGameBoardTemp),
    change_board_element(NewGameBoardTemp, NewRow, NewColumn, PlayerTurn, NewGameBoardTemp2),
    checkCapture([NewGameBoardTemp2|PlayerTurn], [NewRow|NewColumn], ListOfCaptures),
    removeCapturedPieces(NewGameBoardTemp2, ListOfCaptures, NewGameBoard),
    log_movement_msg(PlayerTurn, Row,Column,NewRow,NewColumn),
    %   MISSING FUNCTION HERE! Check if there is a capture
    change_player_turn(PlayerTurn, NewPlayerTurn),
    game_over(NewGameBoard, NewPlayerTurn),
    NewGameState = [NewGameBoard|NewPlayerTurn].

% change_board_element([[w,w,w,w,w],[e,e,e,e,e],[e,e,e,e,e],[e,e,e,e,e],[b,b,b,b,b]], 1,3,e,R).

% valid_moves(+GameState, -ListOfMoves)
% Returns all valid moves with the game state given (game board and player turn).
valid_moves([GameBoard|PlayerTurn], ListOfMoves):-
    game_board_size(BoardSize),
    element_on_board_moves(GameBoard, BoardSize, PlayerTurn, 1, 1, [], ListOfMoves).
    % length(ListOfMoves, Size),
    % write(Size).

% choose_move(+GameState, +Level, -Move)
% @description: the move that the computer takes, which depends on the level of difficulty
%               - level 1 - random valid move
%               - level 2 - best move for the current state (greedy algorithm)  
choose_move([GameBoard|PlayerTurn], 1, Move):-
    valid_moves([GameBoard|PlayerTurn], ListOfMoves),
    length(ListOfMoves, Size),
    random(0, Size, RandomMoveIndex),
    nth0(RandomMoveIndex, ListOfMoves, Move).
    

game_over(GameBoard, OtherPlayer):- 
    count_list(OtherPlayer, GameBoard, Pawns),      % Checks if game over condition is met
    Pawns < 2,      % If game over condition is achieved
    !,
    retract(game_over(false)),
    asserta(game_over(true)),nl,
    change_player_turn(OtherPlayer, NewPlayerTurn),
    format("~s player win !", [NewPlayerTurn] ), nl.

game_over(_,_). %If not game over

