% play:-


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
    create_row(NewSize, Type, NewCurrentRow, Result).


