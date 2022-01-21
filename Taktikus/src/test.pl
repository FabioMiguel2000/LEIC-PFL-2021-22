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

test2:-
    put_code(9556), put_code(9552), put_code(9552),put_code(9552),put_code(9552),put_code(9552),put_code(9574),nl,
    put_code(9553), write('  A  '), put_code(9553),nl,
    % put_code(9553), write('   '), put_code(9553),nl,
    % put_code(9553), write('   '), put_code(9553),nl,
    put_code(9562), put_code(9552), put_code(9552),put_code(9552),put_code(9552),put_code(9552),put_code(9565),nl.




