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
|black).

gamestate_example_3([
[[empty,empty,empty,empty,white,empty,empty,empty],
[empty,empty,black,empty,empty,empty,empty,empty],
[empty,white,empty,white,empty,empty,empty,empty],
[empty,empty,empty,empty,empty,empty,empty,empty],
[empty,empty,empty,empty,empty,empty,empty,empty],
[empty,empty,empty,empty,empty,empty,empty,empty],
[empty,empty,empty,empty,empty,empty,empty,empty],
[empty,empty,black,empty,empty,empty,empty,empty]]
|black).

gamestate_example_4([
[[empty,empty,empty,empty,empty,empty,empty,empty],
[empty,empty,black,empty,empty,empty,empty,empty],
[empty,white,empty,empty,empty,empty,empty,empty],
[empty,white,empty,empty,empty,empty,empty,empty],
[empty,white,empty,empty,empty,empty,empty,empty],
[empty,black,empty,empty,empty,empty,empty,empty],
[empty,empty,empty,empty,empty,empty,empty,empty],
[empty,empty,empty,empty,empty,empty,empty,empty]]
|black).


gamestate_example_5([
[[empty,empty,empty,empty,empty,empty,empty,empty],
[empty,white,black,empty,empty,empty,empty,empty],
[empty,empty,empty,white,empty,empty,empty,empty],
[empty,empty,empty,empty,empty,empty,empty,empty],
[empty,empty,empty,empty,empty,empty,empty,empty],
[empty,empty,empty,empty,empty,empty,empty,empty],
[empty,empty,empty,empty,empty,empty,empty,empty],
[empty,empty,empty,empty,empty,empty,empty,empty]]
|white).





test2:-
    initital_game_board(GB),
    display_board(GB).





