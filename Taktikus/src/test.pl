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

% display_white_piece:-
%     write('  '),
%     put_code(10112),
%     write('  ').

% display_black_piece:-
%     write('  '),
%     put_code(10122),
%     write('  ').

display_element(white):-
    write('  '),
    put_code(10112),
    write('  ').
display_element(black):-
    write('  '),
    put_code(10122),
    write('  ').

display_element(empty):-
    write('     ').

display_row_number(Num):-
    write('  '),
    write(Num),
    write('  ').

display_col_character(0):-
    write('     ').

display_col_character(ColNum):-
    Code is ColNum + 96,
    write('  '),
    put_code(Code),
    write('  ').

display_top_divider(1, BoardSize):-
    !,
    put_code(9556), put_code(9552), put_code(9552), put_code(9552),put_code(9552),put_code(9552),
    put_code(9574), put_code(9552), put_code(9552),put_code(9552),put_code(9552),put_code(9552),
    display_top_divider(2, BoardSize).

display_top_divider(BoardSize, BoardSize):-
    !,
    put_code(9574), put_code(9552), put_code(9552), put_code(9552),put_code(9552),put_code(9552),put_code(9559),nl.

display_top_divider(Index, BoardSize):-
    put_code(9574), put_code(9552), put_code(9552),put_code(9552),put_code(9552),put_code(9552),
    Index2 is Index + 1,
    display_top_divider(Index2, BoardSize).

display_col_character_row(0):-
    !,
    put_code(9553), display_col_character(0),
    put_code(9553), display_col_character(1),
    display_col_character_row(2).

display_col_character_row(Count):-
    game_board_size(BoardSize),
    BoardSize < Count,
    !,
    put_code(9553),nl.
    

display_col_character_row(Count):-
    put_code(9553), display_col_character(Count),
    Count2 is Count + 1,
    display_col_character_row(Count2).


display_element_row([], _):-
    !,
    put_code(9553),nl.

display_element_row([Ele|Rest], RowNumber):-
    game_board_size(BoardSize),
    length([Ele|Rest], CurrentSize),
    BoardSize = CurrentSize,
    !,
    put_code(9553), display_row_number(RowNumber),
    put_code(9553), display_element(Ele),
    display_element_row(Rest, RowNumber).

display_element_row([Ele|Rest], _):-
    put_code(9553), display_element(Ele),
    display_element_row(Rest).

display_middle_divider(1, BoardSize):-
    !,
    put_code(9568), put_code(9552), put_code(9552),put_code(9552),put_code(9552),put_code(9552),
    put_code(9580), put_code(9552), put_code(9552),put_code(9552),put_code(9552),put_code(9552),
    display_middle_divider(2, BoardSize).


display_middle_divider(BoardSize, BoardSize):-
    !,
    put_code(9580), put_code(9552), put_code(9552), put_code(9552),put_code(9552),put_code(9552),put_code(9571),nl.


display_middle_divider(Index, BoardSize):-
    put_code(9580), put_code(9552), put_code(9552),put_code(9552),put_code(9552),put_code(9552),
    Index2 is Index + 1,
    display_middle_divider(Index2, BoardSize).


display_bottom_divider(BoardSize, BoardSize):-
    !,
    put_code(9577), put_code(9552), put_code(9552), put_code(9552),put_code(9552),put_code(9552),put_code(9565),nl.

display_bottom_divider(1, BoardSize):-
    !,
    put_code(9562), put_code(9552), put_code(9552), put_code(9552),put_code(9552),put_code(9552),
    put_code(9577), put_code(9552), put_code(9552),put_code(9552),put_code(9552),put_code(9552),
    display_bottom_divider(2, BoardSize).

display_bottom_divider(Index, BoardSize):-
    put_code(9577), put_code(9552), put_code(9552), put_code(9552),put_code(9552),put_code(9552),
    Index2 is Index + 1,
    display_bottom_divider(Index2, BoardSize).

display_first_row_cells(Row, BoardSize, RowNumber):-
    display_top_divider(1, BoardSize),
    display_col_character_row(0),
    display_middle_divider(1, BoardSize),
    display_element_row(Row, RowNumber).

display_middle_row_cells(Row, BoardSize, RowNumber):-
    display_middle_divider(1, BoardSize),
    display_element_row(Row, RowNumber).

display_bottom_row_cells(Row, BoardSize, RowNumber):-
    display_middle_divider(1, BoardSize),
    display_element_row(Row, RowNumber),
    display_bottom_divider(1, BoardSize).

display_board([LastRow|[]], RowNumber):-   %for the last row
    !,
    game_board_size(BoardSize),
    display_bottom_row_cells(LastRow, BoardSize, RowNumber).

display_board([BoardRow|Rest], 1):-    % for the first row
    game_board_size(BoardSize),
    !,
    display_first_row_cells(BoardRow, BoardSize, 1),
    display_board(Rest, 2).

display_board([BoardRow|Rest], RowNumber):-
    game_board_size(BoardSize),
    display_middle_row_cells(BoardRow, BoardSize, RowNumber),
    RowNumber2 is RowNumber +1,
    display_board(Rest, RowNumber2).

initital_game_board([[white,white,white,white,white,white,white,white],
[empty,empty,empty,empty,empty,empty,empty,empty],
[empty,empty,empty,empty,empty,empty,empty,empty],
[empty,empty,empty,empty,empty,empty,empty,empty],
[empty,empty,empty,empty,empty,empty,empty,empty],
[empty,empty,empty,empty,empty,empty,empty,empty],
[empty,empty,empty,empty,empty,empty,empty,empty],
[black,black,black,black,black,black,black,black]]).

add_row_num_to_board([], _, AccBoard, AccBoard).

add_row_num_to_board([BoardRow|Rest], RowNumber, AccBoard, NewBoard):-
    append([RowNumber],BoardRow, NewBoardRow),
    RowNumber2 is RowNumber + 1,
    append(AccBoard, [NewBoardRow], AccBoard2),
    add_row_num_to_board(Rest, RowNumber2, AccBoard2, NewBoard).

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

test2:-

    initital_game_board(GB),
    % add_row_num_to_board(GB, 1, [], GB1),
    % length(GB1, RowSize),
    % add_col_char_to_board(GB1, RowSize, 1, [], GB2),
    % write(GB2).
    display_board(GB2, 1).





