game_board_size(8).
ui(black, 9823).
ui(white, 9817).
ui(empty, 9634).

% main function that begins the game
play:-
    game_board_size(Size),
    initial_state(Size, GameState),
    display_game(GameState).

% Displays the gameBoard given by <GameState>
% display_game(+GameState)
display_game([GameBoard|PlayerTurn]):-
    write('\n\n'),
    display_board(GameBoard),
    write('\n\n'),
    display_player_turn(PlayerTurn).

% visually outputs on the CLI the current player to make the move
display_player_turn(PlayerTurn):-
    write('Current Turn '),
    put_code(9758),
    format(' ~w ', PlayerTurn),
    ui(PlayerTurn, Code),
    put_code(Code),
    write('\n').

% visually outputs on the CLI the gameboard
display_board(GameBoard):-
    aux_display_board(GameBoard, 1).

% outputs the row numbers (A, B, C, D, E ...)
display_row_description(CurrentRowNumber, RowNumber):-
    CurrentRowNumber = RowNumber,
    write('\n').

display_row_description(CurrentRowNumber, RowNumber):-
    \+ CurrentRowNumber > RowNumber,
    CodeNum is CurrentRowNumber + 64,
    NewCurrentRowNumber is CurrentRowNumber + 1,
    put_code(CodeNum),
    write(' '),
    display_row_description(NewCurrentRowNumber, RowNumber).

% auxiliar function that helps visually outputs on the CLI a the gameboard
aux_display_board([], RowNumber):-
    write('    '),
    display_row_description(1, RowNumber).

aux_display_board([Row|Rest], RowNumber):-
    write('  '),
    write(RowNumber),
    NextRowNumber is RowNumber + 1,
    write(' '),
    display_row(Row),
    aux_display_board(Rest, NextRowNumber).

% visually outputs on the CLI a particular row of a gameboard
display_row([]):-
    write('\n').

display_row([Element|Rest]):-
    display_cell(Element),
    write(' '),
    display_row(Rest).

% visually outputs on the CLI the element ui
display_cell(Element):-
    ui(Element, ElementCode),
    put_code(ElementCode).


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
    
% creates and returns the initial gameboard with a given <Size>
create_board(Size, Result):-
    EmptyRowsNum is Size -2,
    create_empty_board(EmptyRowsNum, Size, EmptyRows),
    create_row(Size, white, WhiteRow),
    create_row(Size, black, BlackRow),
    append([WhiteRow], EmptyRows, BoardWithWhitePieces),
    append(BoardWithWhitePieces, [BlackRow], Result).

% creates and returns the empty side of the board
create_empty_board(Rows, Columns, Result):-
    create_row(Columns, empty, EmptyRow),
    aux_create_empty_board(Rows, EmptyRow, [], Result).

% auxiliar function that creates and returns the empty side of the board
aux_create_empty_board(Size, EmptyRow, CurrentBoard, Result):-
    Size =:= 1,
    append(CurrentBoard, [EmptyRow], Result).

aux_create_empty_board(Size, EmptyRow, CurrentBoard, Result):-
    Size > 1,
    append(CurrentBoard, [EmptyRow], NewBoard),
    NewSize is Size - 1,
    aux_create_empty_board(NewSize, EmptyRow, NewBoard, Result).

% creates and returns a row with <Size> elements of a fiven <Type>
create_row(Size, Type, Result):-
    aux_create_row(Size, Type, [], Result).

% auxiliar function that creates and returns a row with <Size> elements of a fiven <Type>
aux_create_row(0, _, _, []).

aux_create_row(Size, Type, CurrentRow, Result):-
    Size =:= 1,
    append(CurrentRow, [Type], Result).

aux_create_row(Size, Type, CurrentRow, Result):-
    Size > 1,
    append(CurrentRow, [Type], NewCurrentRow),
    NewSize is Size - 1,
    aux_create_row(NewSize, Type, NewCurrentRow, Result).


