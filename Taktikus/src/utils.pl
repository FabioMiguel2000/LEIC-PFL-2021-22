numberLetter(1, 'A').
numberLetter(1, 'a').
numberLetter(1, '1').

numberLetter(2, 'B').
numberLetter(2, 'b').
numberLetter(2, '2').

numberLetter(3, 'C').
numberLetter(3, 'c').
numberLetter(3, '3').

numberLetter(4, 'D').
numberLetter(4, 'd').
numberLetter(4, '4').


numberLetter(5, 'E').
numberLetter(5, 'e').
numberLetter(5, '5').

numberLetter(6, 'F').
numberLetter(6, 'f').
numberLetter(6, '6').


numberLetter(7, 'G').
numberLetter(7, 'g').
numberLetter(7, '7').

numberLetter(8, 'H').
numberLetter(8, 'h').
numberLetter(8, '8').

numberLetter(9, 'I').
numberLetter(9, 'i').
numberLetter(9, '9').

% Convert ASCII code to its respective decimal number
ascii_convert(48, 0).
ascii_convert(49, 1).
ascii_convert(50, 2).
ascii_convert(51, 3).
ascii_convert(52, 4).
ascii_convert(53, 5).
ascii_convert(54, 6).
ascii_convert(55, 7).
ascii_convert(56, 8).
ascii_convert(57, 9).

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
read_number(Min,Max,Number):-
    format('| Choose an Option (~d-~d) - ', [Min, Max]),
    get_code(Ascii),
    peek_char(Char),
    Char == '\n',
    ascii_convert(Ascii, Number),
    Number =< Max, Number >= Min.

read_number_board(Min,Max,Number):-
    format('| Choose an Option (~d-~d) - ', [Min, Max]),
    get_code(Ascii),
    peek_char(Char),
    Char == '\n',
    ascii_convert(Ascii, Number),
    Number =< Max, Number >= Min.


read_number_board(Min,Max,Number):- nl,
    write('Not a valid number, try again\n'),
    format('| Valid Numbers are in the range (~d-~d) - ', [Min+3, Max]),nl,
    read_number(Min, Max, Number).

read_number(Min,Max,Number):- nl,
    write('Not a valid number, try again\n'),
    format('| Valid Numbers are in the range (~d-~d) - ', [Min, Max]),nl,
    read_number(Min, Max, Number).