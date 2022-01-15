% 1. Funcionamento do Cut
% Considere o seguinte código:
s(1).
s(2):- !.
s(3).
% Sem usar o interpretador, indique qual o resultado de cada uma das seguintes interrogações:
% a) | ?- s(X).
% b) | ?- s(X), s(Y).
% c) | ?- s(X), !, s(Y).


% a) X = 1
% b) X = 1, Y = 1
% c) X = 1, Y = 1


% 2. Efeito do Cut
% Considere o seguinte código.
data(one).
data(two).
data(three).
cut_test_a(X):- data(X).
cut_test_a('five').
cut_test_b(X):- data(X), !.
cut_test_b('five').
cut_test_c(X, Y):- data(X), !, data(Y).
cut_test_c('five', 'five').
% Sem usar o interpretador, indique o resultado de cada uma das seguintes interrogações.
% a) | ?- cut_test_a(X), write(X), nl, fail.
% b) | ?- cut_test_b(X), write(X), nl, fail.
% c) | ?- cut_test_c(X, Y), write(X-Y), nl, fail.



% a) 
% one
% two
% three
% five
% no

% b) 
% one
% no

% c)
% one-one
% one-two
% one-three
% no




% 3. Cuts Vermelhos e Verdes
% Indique, justificando, se cada um dos cuts presentes no seguinte código é verde ou vermelho.
%
adult(ben).
adult(john).

immature(X):- adult(X), !,fail.                                
immature(X).
% Cut Vermelho, cut funciona para a logica do programa,
% ao remover do programa, todos os parametros intanciados para o immature vao retornar yes.

% Com cut
% | ?- immature(john).
%         1      1 Call: immature(john) ?
%         2      2 Call: adult(john) ?
%         2      2 Exit: adult(john) ?
%         1      1 Fail: immature(john) ?
% no

% Sem cut
% | ?- immature(john).
%         1      1 Call: immature(john) ?
%         2      2 Call: adult(john) ?
%         2      2 Exit: adult(john) ?
%         1      1 Exit: immature(john) ?
% yes

% adult(X):- person(X), !, age(X, N), N >=18.                       
% adult(X):- turtle(X), !, age(X, N), N >=50.
% adult(X):- spider(X), !, age(X, N), N>=1.
% adult(X):- bat(X), !, age(X, N), N >=5.

% Cut Verde, cut funciona para melhorar a eficiencia do programa, 
% para que a operacao funcione como se fosse um 'switch' e o cut funciona como se fosse um 'break'



% 4. Entrada e Saída de Dados
% Implemente as seguintes alíneas sem usar o predicado format/2.
% a) Implemente o predicado print_n(+S, +N) que imprime N vezes na consola o símbolo S.

print_n(_,0):- !.
print_n(S,N):-
    write(S),
    N1 is N -1,
    print_n(S, N1).

% b) Implemente o predicado print_text(+Text, +Symbol, +Padding) que imprime o texto
% presente no primeiro argumento (usando aspas) com o padding indicado no terceiro
% argumento (número de espaços antes e depois do texto), rodeado de Symbol. Ex.:
% | ?- print_text("Olá Mundo!", '*', 4).
% *    Olá Mundo    *


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

% c) Implemente o predicado print_banner(+Text, +Symbol, +Padding) que imprime o texto
% presente no primeiro argumento (usando aspas) com o formato do exemplo abaixo:
% | ?- print_banner("Olá Mundo!", '*', 4).
% *******************
% *                 *
% *    Olá Mundo    *
% *                 *
% *******************

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

% d) Implemente o predicado read_number(-X) que lê um número da entrada padrão, dígito a
% dígito (ie, sem usar read), devolvendo esse número (como inteiro). Sugestão: use
% peek_code para determinar quando terminar a leitura (o código ASCCI de Line Feed é 10).
% e) Implemente o predicado read_until_between(+Min, +Max, -Value), que peça ao utilizador
% para inserir um inteiro entre Min e Max, e retorne apenas quando o valor inserido estiver
% entre esses limites. Dica: garanta que o predicado read_number/1 é determinista.
% f) Implemente o predicado read_string(-X) que lê uma cadeia de caracteres da entrada
% padrão, caráter a caráter, devolvendo uma string (ie, uma lista de códigos ASCII).
% g) Implemente o predicado banner/0 que pede ao utilizador os argumentos a usar numa
% chamada ao predicado print_banner/3, lê esses argumentos, e invoca o predicado.
% h) Implemente o predicado print_multi_banner(+ListOfTexts, +Symbol, +Padding) que
% imprime várias linhas de texto no formato de um banner, usando a linha de maior
% comprimento para cálculo do padding a usar nas restantes.
% | ?- print_multi_banner(["Frase um", "Frase Dois"], '*', 4).
% ********************
% * *
% * Frase um *
% * Frase Dois *
% * *
% ********************
% yes
% | ?- print_multi_banner(["Olá Mundo! ", [73,32,9829,32,80,114,111,
% 108,111,103], [73,116,32,82,117,108,122,33]], '* ', 4).
% ...
% i) Implemente o predicado oh_christmas_tree(+N) que imprime uma árvore de tamanho N.
% | ?- oh_christmas_tree(5).
%      *
%     ***
%    *****
%   *******
%  **********
% 
