
:- include('view.pl').
:- include('board.pl').
:- include('utils.pl').

:- dynamic(game_over/1).

game_over(false).

% main function that begins the game
play:-
    game_board_size(Size),
    initial_state(Size, GameState),
    game_loop(GameState).

game_loop(GameState):-
    game_over(Flag),
    Flag = false,
    display_game(GameState),
    read_user_input(Move),
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

% [[w,w,w,w,w],[e,e,e,e,e],[e,e,e,e,e],[e,e,e,e,e],[b,b,b,b,b]]

change_board_element(GameBoard, Row, Col, NewElement, NewGameBoard):-
    aux_change_board_element(GameBoard, 1, Row, Col, NewElement, [], NewGameBoard).

aux_change_board_element([], _, _, _, _, SavedBoard, NewGameBoard):-
    NewGameBoard = SavedBoard.

% SavedGame starts with []
aux_change_board_element([Row|Rest], CurrentRowNum, RowNum, ColNum, NewElement, SavedBoard, NewGameBoard):-
    \+ CurrentRowNum = RowNum,
    NewRowNum is CurrentRowNum + 1,
    append(SavedBoard, [Row], NewSavedBoard),
    aux_change_board_element(Rest, NewRowNum, RowNum, ColNum, NewElement, NewSavedBoard, NewGameBoard).

aux_change_board_element([Row|Rest], CurrentRowNum, RowNum, ColNum, NewElement, SavedBoard, NewGameBoard):-
    CurrentRowNum = RowNum,
    NewRowNum is CurrentRowNum + 1,
    change_row_element(Row, ColNum, NewElement, NewRow),
    append(SavedBoard, [NewRow], NewSavedBoard),
    aux_change_board_element(Rest, NewRowNum, RowNum, ColNum, NewElement, NewSavedBoard, NewGameBoard).


change_row_element(Row, ColumnNum, NewElement, NewRow):-
    aux_change_row_element(Row, 1, ColumnNum, NewElement, [], NewRow).

aux_change_row_element([], _, _, _, SavedRow, NewRow):-
    NewRow = SavedRow.

aux_change_row_element([Cell|Rest], CurrentCol, ColNum, NewElement, SavedRow, NewRow):-
    \+ CurrentCol = ColNum,
    NewColNum is CurrentCol + 1,
    append(SavedRow, [Cell], NewSavedRow),
    aux_change_row_element(Rest, NewColNum, ColNum, NewElement, NewSavedRow, NewRow).

aux_change_row_element([_|Rest], CurrentCol, ColNum, NewElement, SavedRow, NewRow):-
    CurrentCol = ColNum,
    NewColNum is CurrentCol + 1,
    append(SavedRow, [NewElement], NewSavedRow),
    aux_change_row_element(Rest, NewColNum, ColNum, NewElement, NewSavedRow, NewRow).

% change_player_turn(+PlayerTurn, -NewPlayerTurn)
change_player_turn(black, white).
change_player_turn(white, black).



read_user_input(Move):-
    read(Input),    %expecting something like d4d5.
    atom_chars(Input, InputList),
    parse_input(InputList, Move).

parse_input([Col|[Row|[NewCol|[NewRow]]]], Move):-
    letterNumber(ColNum, Col),
    letterNumber(NewColNum, NewCol),
    letterNumber(RowNum, Row),
    letterNumber(NewRowNum, NewRow),
    Move = [[RowNum,ColNum],[NewRowNum,NewColNum]].


% move(+GameState, +Move, -NewGameState)
%       -   Move = composed of old postion and new postion
%               - Move = [OldPosition|NewPostion]
%               - Position = [RowNum|ColNum], (e.g. [1,5])
move([GameBoard|PlayerTurn], [[Row,Column],[NewRow,NewColumn]], NewGameState):-
    change_board_element(GameBoard, Row, Column, empty, NewGameBoardTemp),
    change_board_element(NewGameBoardTemp, NewRow, NewColumn, PlayerTurn, NewGameBoard),
    change_player_turn(PlayerTurn, NewPlayerTurn),
    NewGameState = [NewGameBoard|NewPlayerTurn].



    

% change_board_element([[w,w,w,w,w],[e,e,e,e,e],[e,e,e,e,e],[e,e,e,e,e],[b,b,b,b,b]], 1,3,e,R).