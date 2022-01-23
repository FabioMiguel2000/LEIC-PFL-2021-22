ui(black, 10123).
ui(white, 10112).
ui(empty, 43).

:- dynamic(move_counter/1).

move_counter(1).

% Displays the gameBoard given by <GameState>
% display_game(+GameState)
display_game([GameBoard|PlayerTurn]):-
    write('\n\n'),
    display_board(GameBoard),
    write('\n\n'),
    display_player_turn(PlayerTurn).


% visually outputs on the CLI the current player to make the move
display_player_turn(black):-
    print_banner("Current Turn => black", '*', 16),nl,nl.

display_player_turn(white):-
    print_banner("Current Turn => white", '*', 16),nl,nl.

% display(+ElementType)
% @description: displays the individual element on the board cell, 
%               this also includes for table description (table row number + table column letter)
display_element(white):-
    write('  '),
    ui(white, Code),
    put_code(Code),
    write('  ').

display_element(black):-
    write('  '),
    ui(black, Code),
    put_code(Code),
    write('  ').

display_element(empty):-
    write('     ').

display_element(TableNum):-
    write('  '),
    write(TableNum),
    write('  ').


% display_top_divider(+RowSize)
% @description: displays the top side elements of the board with a size of <RowSize>
% e.g. given a <RowSize> = 9, it displays ╔═════╦═════╦═════╦═════╦═════╦═════╦═════╦═════╦═════╗
display_top_divider(RowSize):-
    display_top_divider(1, RowSize).

display_top_divider(1, RowSize):-
    !,
    put_code(9556), put_code(9552), put_code(9552),put_code(9552),put_code(9552),put_code(9552),
    display_top_divider(2, RowSize).

display_top_divider(RowSize, RowSize):-
    !,
    put_code(9574), put_code(9552), put_code(9552), put_code(9552),put_code(9552),put_code(9552),put_code(9559),nl.

display_top_divider(Index, RowSize):-
    put_code(9574), put_code(9552), put_code(9552),put_code(9552),put_code(9552),put_code(9552),
    Index2 is Index + 1,
    display_top_divider(Index2, RowSize).

% display_element_row(+Row)
% @description: displays the elements of a particular row given by <Row> on the board.
% e.g. given a <Row> = [3, white, black, white, empty, black, empty, empty], it displays ║  3  ║  ➀  ║  ➋  ║  ➀  ║     ║  ➋  ║     ║     ║
display_element_row([]):-
    !,
    put_code(9553),nl.

display_element_row([Ele|Rest]):-
    game_board_size(BoardSize),
    length([Ele|Rest], CurrentSize),
    BoardSize = CurrentSize,
    !,
    put_code(9553), display_element(Ele),
    display_element_row(Rest).

display_element_row([Ele|Rest]):-
    put_code(9553), display_element(Ele),
    display_element_row(Rest).

% display_middle_divider(+RowSize)
% @description: displays the horizontal lines of the board that divides each row with a size given by <RowSize>
% e.g. given a <RowSize> = 9, it displays ╠═════╬═════╬═════╬═════╬═════╬═════╬═════╬═════╬═════╣

display_middle_divider(RowSize):-
    display_middle_divider(1, RowSize).

display_middle_divider(1, RowSize):-
    !,
    put_code(9568), put_code(9552), put_code(9552),put_code(9552),put_code(9552),put_code(9552),
    display_middle_divider(2, RowSize).


display_middle_divider(RowSize, RowSize):-
    !,
    put_code(9580), put_code(9552), put_code(9552), put_code(9552),put_code(9552),put_code(9552),put_code(9571),nl.


display_middle_divider(Index, RowSize):-
    put_code(9580), put_code(9552), put_code(9552),put_code(9552),put_code(9552),put_code(9552),
    Index2 is Index + 1,
    display_middle_divider(Index2, RowSize).

% display_bottom_divider(+RowSize)
% @description: displays the bottom side elements of the board with a size of <RowSize>
% e.g. given a <RowSize> = 9, it displays ╚═════╩═════╩═════╩═════╩═════╩═════╩═════╩═════╩═════╝
display_bottom_divider(RowSize):-
    display_bottom_divider(1, RowSize).

display_bottom_divider(RowSize, RowSize):-
    !,
    put_code(9577), put_code(9552), put_code(9552), put_code(9552),put_code(9552),put_code(9552),put_code(9565),nl.

display_bottom_divider(1, RowSize):-
    !,
    put_code(9562), put_code(9552), put_code(9552), put_code(9552),put_code(9552),put_code(9552),
    display_bottom_divider(2, RowSize).

display_bottom_divider(Index, RowSize):-
    put_code(9577), put_code(9552), put_code(9552), put_code(9552),put_code(9552),put_code(9552),
    Index2 is Index + 1,
    display_bottom_divider(Index2, RowSize).

% display_first_row_cells(+Row)
% @description: displays the first upper row of the board
display_first_row_cells(Row):-
    length(Row, RowSize),
    display_top_divider(RowSize),
    display_element_row(Row).

% display_middle_row_cells(+Row)
% @description: displays the inner row of the board, rows that are not the first row and last row
display_middle_row_cells(Row):-
    length(Row, RowSize),
    display_middle_divider(RowSize),
    display_element_row(Row).

% display_bottom_row_cells(+Row)
% @description: displays the last row of the board
display_bottom_row_cells(Row):-
    length(Row, RowSize),
    display_middle_divider(RowSize),
    display_element_row(Row),
    display_bottom_divider(RowSize).

% add_row_num_to_board(+GameBoard, -NewGameBoard)
% @description: adds the row numbers descriptions to the board
add_row_num_to_board(GameBoard, NewBoard):-
    add_row_num_to_board(GameBoard, 1, [], NewBoard).

add_row_num_to_board([], _, AccBoard, AccBoard).

add_row_num_to_board([BoardRow|Rest], RowNumber, AccBoard, NewBoard):-
    append([RowNumber],BoardRow, NewBoardRow),
    RowNumber2 is RowNumber + 1,
    append(AccBoard, [NewBoardRow], AccBoard2),
    add_row_num_to_board(Rest, RowNumber2, AccBoard2, NewBoard).

% add_col_char_to_board(+GameBoard, -NewGameBoard)
% @description: adds the column letters descriptions to the board
add_col_char_to_board(GameBoard, NewBoard):-
    length(GameBoard, RowSize),
    add_col_char_to_board(GameBoard, RowSize, 1, [], NewBoard).

add_col_char_to_board(GameBoard, RowSize, 1, AccRow, NewBoard):-
    !,
    append(AccRow, [empty], AccRow2),
    numberLetter(ColNumber, Letter),
    append(AccRow2, [Letter], AccRow3),
    ColNumber2 is ColNumber + 1,
    add_col_char_to_board(GameBoard, RowSize, ColNumber2, AccRow3, NewBoard).

add_col_char_to_board(GameBoard, RowSize, RowSize, AccRow, NewBoard):-
    !,
    numberLetter(RowSize, Letter),
    append(AccRow, [Letter], AccRow2),
    append([AccRow2], GameBoard, NewBoard).

add_col_char_to_board(GameBoard, RowSize,ColNumber, AccRow, NewBoard):-
    numberLetter(ColNumber, Letter),
    append(AccRow, [Letter], AccRow2),
    ColNumber2 is ColNumber + 1,
    add_col_char_to_board(GameBoard, RowSize, ColNumber2, AccRow2, NewBoard).

add_row_col_number_to_board(GameBoard, NewBoard):-
    add_row_num_to_board(GameBoard, 1, [], GameBoardWithRowsNum),
    add_col_char_to_board(GameBoardWithRowsNum, NewBoard).

% display_board(+GameBoard)
% @description: displays the game board, and adds the row numbers and column letters.
display_board(GameBoard):-
    add_row_col_number_to_board(GameBoard, NewBoard),
    length(NewBoard, Size),
    display_board(NewBoard, Size).

display_board([LastRow|[]], _):-   %for the last row
    !,
    display_bottom_row_cells(LastRow).

display_board([BoardRow|Rest], BoardSize):-    % for the first row
    length([BoardRow|Rest], CurrentSize),
    CurrentSize = BoardSize,
    !,
    display_first_row_cells(BoardRow),
    display_board(Rest, BoardSize).

display_board([BoardRow|Rest], BoardSize):-
    display_middle_row_cells(BoardRow),
    display_board(Rest, BoardSize).


% log_movement_msg(+PlayerTurn,+Row,+Column,+NewRow,+NewColumn)
% Prints the information of the move made
log_movement_msg(PlayerTurn, Row,Column,NewRow,NewColumn):-
    move_counter(MoveNumber),
    MoveNumber2 is MoveNumber + 1,
    retract(move_counter(MoveNumber)),
    asserta(move_counter(MoveNumber2)),
    numberLetter(Column, ColCode),
    numberLetter(NewColumn, NewColCode),
    format('\e[0;32m~n~n~w. ~w moved from ~w~w to ~w~w\e[0;39m', [MoveNumber,PlayerTurn,ColCode, Row, NewColCode,NewRow]).


log_invalid_move:-
    write('\e[0;91m\nWarning: Invalid Move!\e[0;39m\nPlease input in the format:\n<src column><src row><dest column><dest row>, (e.g a1a4)\n\n').


% Below are functions that were made during the PFL pratical lessons, on paper 3 exercise 4.
print_n(_,0):- !.
print_n(S,N):-
    write(S),
    N1 is N -1,
    print_n(S, N1).

print_code_to_text([]):-
    !.
print_code_to_text([H|T]):-
    put_code(H),
    print_code_to_text(T).


print_text(Text, Symbol, Padding):-
    print_n(Symbol, 1),
    print_n(' ', Padding),
    print_code_to_text(Text),
    print_n(' ', Padding),
    print_n(Symbol, 1).

print_top_bot(Text, Symbol, Padding):-
    print_n(Symbol, Padding),
    print_n(Symbol, Padding),
    print_n(Symbol, 2),
    length(Text,TextSize),
    print_n(Symbol,TextSize).

print_empty_sides(Text, Symbol, Padding):-
    write(Symbol),
    print_n(' ', Padding),
    print_n(' ', Padding),
    length(Text,TextSize),
    print_n(' ',TextSize),
    write(Symbol).

print_banner(Text, Symbol, Padding):-
    print_top_bot(Text, Symbol, Padding),
    nl,
    print_empty_sides(Text, Symbol, Padding),
    nl,
    print_text(Text, Symbol, Padding),
    nl,
    print_empty_sides(Text, Symbol, Padding),
    nl,
    print_top_bot(Text, Symbol, Padding).



% ANSI escape code was used
% See here https://gist.github.com/fnky/458719343aabd01cfb17a3a4f7296797
% print('\e[0;47m\e[0;3m').