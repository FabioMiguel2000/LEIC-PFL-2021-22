
factorial(0,1).
factorial(N,F):-
    N > 0,
    N1 is N -1,
    factorial(N1, F1),
    F is N * F1.

fibonacci(0,1).
fibonacci(1,1).
fibonacci(N,F):-
    N >1,
    N1 is N -1,
    N2 is N -2,
    fibonacci(N1, F1),
    fibonacci(N2, F2),
    F is F1 + F2.
    
isPrime(1).
isPrime(2).
isPrime(X):-
    X > 2,
    X1 is X -1,
    auxPrime(X, X1).

auxPrime(X, 1).
auxPrime(X, N):-
    N > 1,
    Remainder is X mod N,
    Remainder > 0,
    N1 is N-1,
    auxPrime(X, N1).

/*
4
a)  T
b)  F
c)  T
d)  H = pfl, T = [lbaw, redes, ltw]
e)  H = lbaw, T = [ltw]
f)  H = leic, T = []
g)  no
h)  H = leic, T = [[pfl, ltw, lbaw, redes]]
i)  H = leic, T = [Two]
j)  Inst = gram, LEIC = feup
k)  One = 1, Two = 2, Tail = [3,4]
l)  One = leic, Rest = [Two,Tail]*/


list_size([H|T], Size):-
    T \= [],
    NewSize is 1,
    list_size(T, NewSize, Size).

list_size([], Size, Return).
list_size([H|T], Size, Return):-
    T \= [],
    NewSize is Size +1,
    Return is Size,
    list_size(T, NewSize, Return).

