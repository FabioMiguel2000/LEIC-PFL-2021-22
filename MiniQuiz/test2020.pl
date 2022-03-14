jogo(1, sporting, porto,1-2).
jogo(1, maritimo, benfica, 2-0).
jogo(2, sporting, benfica,0-2).
jogo(2, porto, maritimo,1-0).
jogo(3, maritimo, sporting, 1-1).
jogo(3, benfica, porto, 0-2).

treinadores(porto, [[1-3]-sergio_conceicao]).
treinadores(sporting, [[1-2]-silas, [3-3]-ruben_amorim]).
treinadores(benfica,[[1-3]-bruno_lage]).
treinadores(maritimo, [[1-3]-jose_gomes]).

%   Pergunta 1

n_treinadores(Equipa, Numero):-
    treinadores(Equipa, Treinadores),
    length(NumeroEncontrado, Treinadores),
    NumeroEncontrado = Numero.


%   Pergunta 2

n_jornadas_treinador(Treinador, NumeroJornadas):-
    n_jornadas_treinador(Treinador, 0, NumeroEncontrado, []),
    NumeroJornadas = NumeroEncontrado.


n_jornadas_treinador(Treinador, Acumulador, NumeroJornadas, EquipasVisitadas):-
    treinadores(Equipa, ListaTreinadores),
    \+ member(Equipa, EquipasVisitadas),
    !,
    n_jornadas_treinador(Treinador, ListaTreinadores, N),
    Acumulador2 is Acumulador + N,
    n_jornadas_treinador(Treinador, Acumulador2, NumeroJornadas, [Equipa|EquipasVisitadas]).

n_jornadas_treinador(_Treinador, Acumulador, Acumulador, _EquipasVisitados).


n_jornadas_treinador(_Treinador, [], 0):-
    !.

n_jornadas_treinador(Treinador, [[JornadaInicial-JornadaFinal]-Treinador| _Lista], NumeroJornadas):-
    !,
    NumeroJornadas is JornadaFinal - JornadaInicial + 1.

n_jornadas_treinador(Treinador, [_OutroTreinador| Lista], NumeroJornadas):-
    n_jornadas_treinador(Treinador, Lista, NumeroJornadas).

%   Pergunta 3
ganhou(Jornada, EquipaVencedora, EquipaDerrotada):-
    jogo(Jornada, EquipaVencedora, EquipaDerrotada, Golos1-Golos2),
    Golos1 > Golos2,
    !.

ganhou(Jornada, EquipaVencedora, EquipaDerrotada):-
    jogo(Jornada, EquipaDerrotada, EquipaVencedora, Golos1-Golos2),
    Golos2 > Golos1.


% ?- o benfica venceu o sporting.
% yes
% ? - o porto venceu o X.
% X = maritimo ? ;
% X = sporting? ;
% X = benfica? ;



% Pergunta 4
% Resposta: c :- op(180, fx, o).
% Olhando so para o segundo termo de entrada (tipo de operador), 'o' seria um operador prefixo, nesse caso ficariamos so com as opcoes c e f, 
% como nao e' um operador associativo, ficariamos com a opcao c.


% Pergunta 5
% Reposta: e :- op(200, xfx, venceu)
% olhando para o facto, podiamos concluir que o significado do facto seria igual com: (o benfica) venceu (o sporting), concluindo que 'venceu' teria um valor de precedencia maior
% do que 180 (pergunta anterior), ou seja 200 a 1200, como 1200 nao fazia sentido neste contexto (pois ia ter a mesma precedencia com :-), entao restavamos as opcoes b, e, que seria
% facil de concluir que 'venceu' e' um operador infixo, entao resposta seria opcao e.



% Pergunta 7

predX(N,N, _).
predX(N, A, B):-
    !,
    A \= B,
    A1 is A + sign(B-A),
    predX(N, A1, B).

% No caso de receber o N instanciado, verifica se o N fica dentro do intervalo [A,B]
% No caso de receber o N nao instanciado, N comeca com o valor de A e permite depois obter todos os valores ate ao B.

% Cut verde, neste caso o cut serve so para aumentar a eficiencia do codigo, para prevenir backtracking desnecessarios.


% Pergunta 8
% Nunca perdeu = empate ou venceu

treinadorNaLista(Treinador, [[Jornada]-Treinador| _Resto], Jornada).
treinadorNaLista(Treinador, [_OutroTreinador| Resto], Jornada):- treinadorNaLista(Treinador, Resto, Jornada).

bom_treinador(Treinador):-
    treinadores(Equipa, Treinadores),
    treinadorNaLista(Treinador, Treinadores, JornadaInicial-JornadaFinal),
    !,
    nunca_perdeu(JornadaInicial, JornadaFinal, Equipa). % sucesso se nunca foi derrotado

nunca_perdeu(Jornada, JornadaFinal, _Equipa):-
    JornadaFinal < Jornada,
    !.

nunca_perdeu(Jornada, JornadaFinal, Equipa):-
    \+ ganhou(Jornada, _, Equipa), % A equipa nao perdeu
    !,
    Jornada2 is Jornada + 1,
    nunca_perdeu(Jornada2, JornadaFinal, Equipa).

nunca_perdeu(_Jornada, _JornadaFinal, _Equipa):- % a equipa perdeu pelo menos uma fez
    fail.



imprime_totobola(1,'1').
imprime_totobola(0,'x').
imprime_totobola(-1,'2').
imprime_texto(X, 'vitoria da casa'):-
    X = 1.
imprime_texto(X, 'empate') :-
    X = 0.
imprime_texto(X, 'derrota da casa') :-
    X = -1.

% Pergunta 9

imprime_jogos(F):-
    jogo(Jornada, EquipaDeCasa, EquipaVisitante, Golos1-Golos2),
    format('Jornada ~d: ~w x ~w - ', [Jornada, EquipaDeCasa, EquipaVisitante]),
    quem_vence(Golos1-Golos2, X),
    Imprime=..[F, X, Info],
    Imprime,
    write(Info),nl,
    fail.

imprime_jogos(_F).


quem_vence(Golos-Golos, 0).
quem_vence(Golos1-Golos2, 1):-
    Golos1 > Golos2.

quem_vence(Golos1-Golos2, -1):-
    Golos2 > Golos1.


% Pergunta 10

% Resposta: e

% Pergunta 11

% Resposta: e

% Pergunta 12

treinadorNaLista(NomeTreinador, [_Jornada-NomeTreinador|_Resto]).
treinadorNaLista(NomeTreinador, [_Jornada-_NomeTreinador|Resto]):-
    treinadorNaLista(NomeTreinador, Resto).


lista_treinadores(L):-
    findall(NomeTreinador, (treinadores(_Equipa, ListaTreinadores), treinadorNaLista(NomeTreinador, ListaTreinadores)), L).
    
    
% Pergunta 13

:- use_module(library(lists)).

treinadorNaLista13(NomeTreinador, [[JornadaInicial-JornadaFinal]-NomeTreinador|_Resto],JornadaInicial-JornadaFinal).
treinadorNaLista13(NomeTreinador, [_Jornada-_NomeTreinador|Resto], JornadaInicial-JornadaFinal):-
    treinadorNaLista13(NomeTreinador, Resto, JornadaInicial-JornadaFinal).

duracao_treinadores(L):-
    findall(NumeroJornadas-NomeTreinador, (treinadores(_Equipa, ListaTreinadores), treinadorNaLista13(NomeTreinador, ListaTreinadores, JornadaInicial-JornadaFinal), NumeroJornadas is JornadaFinal - JornadaInicial +1), UnsortedL),
    sort(UnsortedL, UnreversedL),
    reverse(UnreversedL, L).

% Pergunta 14

pascal(1, [1]).

pascal(N, L):-
    N1 is N -1,
    pascal(N1, L1),
    append([0], L1, L2),
    append(L2, [0], L3),
    calculate_new_pascal(L3, [], L).

calculate_new_pascal([_LastElement|[]], Acc, Acc):- !.
calculate_new_pascal([Element1, Element2| Rest], Acc, L):-
    NewElement is Element1 + Element2,
    append(Acc, [NewElement], Acc2),
    calculate_new_pascal([Element2| Rest], Acc2, L).


    
    

    
    
    

