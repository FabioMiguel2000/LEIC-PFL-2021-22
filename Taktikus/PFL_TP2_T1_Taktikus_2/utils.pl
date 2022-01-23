game_board_size(8).     % game_board_size(-N), size of the game board, given by N*N, for Taktikus 8x8 is recommended

numberLetter(1, 'a').
numberLetter(1, 'A').
numberLetter(1, '1').

numberLetter(2, 'b').
numberLetter(2, 'B').
numberLetter(2, '2').

numberLetter(3, 'c').
numberLetter(3, 'C').
numberLetter(3, '3').

numberLetter(4, 'd').
numberLetter(4, 'D').
numberLetter(4, '4').

numberLetter(5, 'e').
numberLetter(5, 'E').
numberLetter(5, '5').

numberLetter(6, 'f').
numberLetter(6, 'F').
numberLetter(6, '6').

numberLetter(7, 'g').
numberLetter(7, 'G').
numberLetter(7, '7').

numberLetter(8, 'h').
numberLetter(8, 'H').
numberLetter(8, '8').

numberLetter(9, 'i').
numberLetter(9, 'I').
numberLetter(9, '9').

count(_, [], 0).
count(E, [E | T], N) :- count(E, T, NT),
                        N is NT + 1.
count(E, [H | T], N) :- H \= E,
                        count(E, T, N).

count_list(_, [], 0).
count_list(E, [H|T], N) :-
    count(E, H, HN),
    count_list(E, T, TN),
    N is HN + TN.

% read_number(_,_,_):.
% Read the number from whithin a menu range.
read_number(Min,Max,Number):-
    format('Option [~d-~d]: ', [Min, Max]),
    read(Number),nl,
    number(Number),
    Number =< Max, Number >= Min.

% if the user type a wrong Number from the current menu ask again for the input.
read_number(Min,Max,Number):- 
    write('\e[0;91m\nInvalid option! \nChoose an option from the current menu\n\e[0;39m'),nl,
    read_number(Min, Max, Number).

% index_increment_by_direction(+DirectionType, +RowIndex, +ColIndex, -RowIndex2, ColIndex2)
% Depending on the type of direction, increments the index to loop through the board.

index_increment_by_direction(top, 1, _, 0, 0):-
    !.
index_increment_by_direction(top, RowIndex, ColIndex, RowIndex2, ColIndex):-
    !,RowIndex2 is RowIndex - 1.


index_increment_by_direction(bottom, RowIndex, _, RowIndex2, ColIndex2):-
    game_board_size(BoardSize),
    RowIndex = BoardSize,
    !,
    RowIndex2 = 0,
    ColIndex2 = 0.

index_increment_by_direction(bottom, RowIndex, ColIndex, RowIndex2, ColIndex):-
    !,RowIndex2 is RowIndex + 1.

index_increment_by_direction(right, _, ColIndex, RowIndex2, ColIndex2):-
    game_board_size(BoardSize),
    ColIndex = BoardSize,
    !,
    RowIndex2 = 0,
    ColIndex2 = 0.

index_increment_by_direction(right, RowIndex, ColIndex, RowIndex, ColIndex2):-
    !,ColIndex2 is ColIndex + 1.


index_increment_by_direction(left, _, 1, 0, 0):-
    !.

index_increment_by_direction(left, RowIndex, ColIndex, RowIndex, ColIndex2):-
    !,ColIndex2 is ColIndex - 1.


% Example game states for testing purposes
gamestate_example_1([
[[white,white,white,white,white,white,white,white],
[empty,empty,empty,empty,empty,empty,empty,empty],
[empty,empty,empty,empty,empty,empty,empty,empty],
[empty,empty,empty,empty,empty,empty,empty,empty],
[empty,empty,empty,empty,empty,empty,empty,empty],
[empty,empty,empty,empty,empty,empty,empty,empty],
[empty,empty,empty,empty,empty,empty,empty,empty],
[black,black,black,black,black,black,black,black]]
|white]).

gamestate_example_2([
[[empty,white,empty,empty,empty,empty,empty,empty],
[empty,empty,empty,black,empty,empty,empty,empty],
[empty,white,empty,empty,empty,empty,empty,empty],
[empty,empty,empty,empty,empty,empty,empty,empty],
[empty,empty,empty,empty,empty,empty,empty,empty],
[empty,empty,empty,empty,empty,empty,empty,empty],
[empty,empty,empty,empty,empty,empty,empty,empty],
[empty,empty,black,empty,empty,empty,empty,empty]]
|black]).

gamestate_example_3([
[[empty,empty,empty,empty,white,empty,empty,empty],
[empty,empty,black,empty,empty,empty,empty,empty],
[empty,white,empty,white,empty,empty,empty,empty],
[empty,empty,empty,empty,empty,empty,empty,empty],
[empty,empty,empty,empty,empty,empty,empty,empty],
[empty,empty,empty,empty,empty,empty,empty,empty],
[empty,empty,empty,empty,empty,empty,empty,empty],
[empty,empty,black,empty,empty,empty,empty,empty]]
|black]).


gamestate_example_4([
[[empty,empty,empty,empty,empty,empty,empty,empty],
[empty,empty,black,empty,empty,empty,empty,empty],
[empty,white,empty,empty,empty,empty,empty,empty],
[empty,white,empty,empty,empty,empty,empty,empty],
[empty,white,empty,empty,empty,empty,empty,empty],
[empty,black,empty,empty,empty,empty,empty,empty],
[empty,empty,empty,empty,empty,empty,empty,empty],
[empty,empty,empty,empty,empty,empty,empty,empty]]
|black]).


gamestate_example_5([
[[empty,empty,empty,empty,empty,empty,empty,empty],
[empty,white,black,empty,empty,empty,empty,empty],
[empty,empty,empty,white,empty,empty,empty,empty],
[empty,empty,empty,empty,empty,empty,empty,empty],
[empty,empty,empty,empty,empty,empty,empty,empty],
[empty,empty,empty,empty,empty,empty,empty,empty],
[empty,empty,empty,empty,empty,empty,empty,empty],
[empty,empty,empty,empty,empty,empty,empty,empty]]
|white]).
