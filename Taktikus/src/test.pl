:- reconsult('utils.pl').

test(X):-
    repeat,
    read(X),
    X = 0.


get_input:-
    read(X),
    length(X, Length),
    write(X),nl,
    write(Length).
    

test:-
    write('             A         B\n'),
    nl,
    write('        + - - - - + - - - - +\n'),
    write('        |         |         |\n'),
    write('   1    |    W    |    B    |\n'),
    write('        |         |         |\n'),
    write('        + - - - - + - - - - +\n').


display_element(white):-
    write('  '),
    put_code(10112),
    write('  ').
display_element(black):-
    write('  '),
    put_code(10123),
    write('  ').

display_element(empty):-
    write('     ').

display_element(TableNum):-
    write('  '),
    write(TableNum),
    write('  ').

display_top_divider(1, BoardSize):-
    !,
    put_code(9556), put_code(9552), put_code(9552),put_code(9552),put_code(9552),put_code(9552),
    display_top_divider(2, BoardSize).

display_top_divider(BoardSize, BoardSize):-
    !,
    put_code(9574), put_code(9552), put_code(9552), put_code(9552),put_code(9552),put_code(9552),put_code(9559),nl.

display_top_divider(Index, BoardSize):-
    put_code(9574), put_code(9552), put_code(9552),put_code(9552),put_code(9552),put_code(9552),
    Index2 is Index + 1,
    display_top_divider(Index2, BoardSize).


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

display_first_row_cells(Row):-
    length(Row, RowSize),
    display_top_divider(1, RowSize),
    display_element_row(Row).

display_middle_row_cells(Row):-
    length(Row, RowSize),
    display_middle_divider(1, RowSize),
    display_element_row(Row).

display_bottom_row_cells(Row):-
    length(Row, RowSize),
    display_middle_divider(1, RowSize),
    display_element_row(Row),
    display_bottom_divider(1, RowSize).

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

initital_game_board([[white,white,white,white,white,white,white,white],
[empty,empty,empty,empty,empty,empty,empty,empty],
[empty,empty,empty,empty,empty,empty,empty,empty],
[empty,empty,empty,empty,empty,empty,empty,empty],
[empty,empty,empty,empty,empty,black,empty,empty],
[empty,empty,empty,empty,empty,empty,empty,empty],
[empty,empty,empty,empty,empty,empty,empty,empty],
[black,black,black,black,black,black,black,black]]).

add_row_num_to_board(GameBoard, NewBoard):-
    add_row_num_to_board(GameBoard, 1, [], NewBoard).

add_row_num_to_board([], _, AccBoard, AccBoard).

add_row_num_to_board([BoardRow|Rest], RowNumber, AccBoard, NewBoard):-
    append([RowNumber],BoardRow, NewBoardRow),
    RowNumber2 is RowNumber + 1,
    append(AccBoard, [NewBoardRow], AccBoard2),
    add_row_num_to_board(Rest, RowNumber2, AccBoard2, NewBoard).

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


test2:-
    initital_game_board(GB),
    display_board(GB).





