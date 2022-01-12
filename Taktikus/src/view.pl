ui(black, 9823).
ui(white, 9817).
ui(empty, 9634).

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

% outputs the column letters (A, B, C, D, E ...)
display_column_letters(CurrentRowNumber, RowNumber):-
    CurrentRowNumber = RowNumber,
    write('\n').

display_column_letters(CurrentRowNumber, RowNumber):-
    \+ CurrentRowNumber > RowNumber,
    CodeNum is CurrentRowNumber + 64,
    NewCurrentRowNumber is CurrentRowNumber + 1,
    put_code(CodeNum),
    write(' '),
    display_column_letters(NewCurrentRowNumber, RowNumber).

% auxiliar function that helps visually outputs on the CLI a the gameboard
aux_display_board([], RowNumber):-
    write('    '),
    display_column_letters(1, RowNumber).

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