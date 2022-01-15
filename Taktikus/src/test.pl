test:-
    get_char(X),
    get_char(Y),
    get_char(Z).


get_input:-
    read(X),
    length(X, Length),
    write(X),nl,
    write(Length).
    
