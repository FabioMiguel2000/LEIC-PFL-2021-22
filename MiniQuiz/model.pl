% No concurso FEUPGotTalent, cada estudante pode participar mostrando as suas
% habilidades num qualquer tema, académico ou extra-curricular. Os interessados
% inscrevem-se, dando o número de estudante, idade e o nome da sua atuação:

% participant(Id,Age,Performance)
participant(1234, 17, 'Pé coxinho').
participant(3423, 21, 'Programar com os pés').
participant(3788, 20, 'Sing a Bit').
participant(4865, 22, 'Pontes de esparguete').
participant(8937, 19, 'Pontes de pen-drives').
participant(2564, 20, 'Moodle hack').


% As atuações são apreciadas por um júri de E elementos.
% Ao longo da atuação (que tem um máximo de 120 segundos), se um elemento do júri
% achar que o participante não deve passar à próxima fase, carrega num botão. Ficam
% registados os tempos em que cada elemento do júri carregou no botão. Se não
% carregou, ficam registados 120 segundos.

%performance(Id,Times)
performance(1234,[120,120,120,120]).
performance(3423,[32,120,45,120]).
performance(3788,[110,2,6,43]).
performance(4865,[120,120,110,120]).
performance(8937,[97,101,105,110]).


% Passam à próxima fase os N participantes que mais se aguentaram em palco, somados
% os tempos de cada elemento do júri, desde que pelo menos um dos elementos do júri
% não tenha carregado no botão.


% Implemente o predicado madeItThrough(+Participant), que sucede se Participant é
% um participante que já atuou e em cuja atuação pelo menos um elemento do júri não
% carregou no botão.

madeItThrough(ParticipantId):-
    performance(ParticipantId, Times),
    checkClicked(Times).

checkClicked([]):-
    !,
    fail.

checkClicked([120|_]).

checkClicked([_|Rest]):-
    checkClicked(Rest).


% Implemente o predicado juriTimes(+Participants, +JuriMember, -Times, -Total), que
% devolve em Times o tempo de atuação de cada participante na lista Participants (pela
% mesma ordem) até que o júri número JuriMember (de 1 a E) carregou no botão, e em
% Total a soma desses tempos.

:-use_module(library(lists)).

juriTimes(Participants, JuriMember, Times, Total):-
    aux_juriTimes(Participants, JuriMember, [], Times, Total).


aux_juriTimes([], _, AccTimes, AccTimes, Total):-
    sumlist(AccTimes, Total).
    
aux_juriTimes([Participant|Rest], JuriMember, AccTimes, Times, Total):-
    performance(Participant, ParticipantTimes),
    nth1(JuriMember, ParticipantTimes, Time),
    append(AccTimes, [Time], AccTimes2),
    aux_juriTimes(Rest, JuriMember, AccTimes2, Times, Total).


% Implemente o predicado patientJuri(+JuriMember) que sucede se o júri JuriMember já
% se absteve de carregar no botão pelo menos por duas vezes.


all_Participant(Participants, AccList):-
    participant(Id, _, _),
    \+ member(Id, AccList),
    append(AccList,[Id], AccList2),
    all_Participant(Participants, AccList2).

all_Participant(AccList, AccList).


patientJuri(JuriMember):-
    all_Participant(Participants, []),
    juriTimes(Participants, JuriMember, Times, _),
    count(Times, 0).

count(_, 2).
    
count([Time|Rest], Count):-
    Time = 120,
    !,
    Count2 is Count + 1,
    count(Rest, Count2).

count([_|Rest], Count):-
    count(Rest, Count).

% Implemente o predicado bestParticipant(+P1, +P2, -P) que unifica P com o melhor dos
% dois participantes P1 e P2. O melhor participante é aquele que tem uma maior soma de
% tempos na sua atuação (independentemente de estar ou não em condições de passar
% à próxima fase). Se ambos tiverem o mesmo tempo total, o predicado deve falhar.
    

bestParticipant(P1, P2, P):-
    performance(P1, Times1),
    performance(P2, Times2),
    sumlist(Times1, TotalTime1),
    sumlist(Times2, TotalTime2),
    (
    (TotalTime1 > TotalTime2, P = P1);
    (TotalTime2 > TotalTime1, P = P2)
    ).

% Implemente o predicado allPerfs, que imprime na consola os números dos
% participantes que já atuaram, juntamente com o nome da sua atuação e lista de
% tempos.


allPerfs:-
    performance(Participant, Times),
    participant(Participant, _age, PerformanceName),
    format('~w:~w:', [Participant, PerformanceName]),
    write(Times),nl,
    fail.

allPerfs.


% Implemente o predicado nSuccessfulParticipants(-T) que determina quantos
% participantes não tiveram qualquer clique no botão durante a sua atuação.

nSuccessfulParticipants(T):-
    all_Participant(Participants, []),
    aux_nSuccessfulParticipants(Participants, 0, T).


aux_nSuccessfulParticipants([], Acc, Acc):-
    !.

aux_nSuccessfulParticipants([Participant|Rest], Acc, Count):-
    performance(Participant, Times),
    sumlist(Times, TotalTimes),
    length(Times, NumberOfJuri),
    MaxTime is NumberOfJuri * 120,
    TotalTimes = MaxTime,
    !,
    Acc2 is Acc +1,
    aux_nSuccessfulParticipants(Rest, Acc2, Count).

aux_nSuccessfulParticipants([_|Rest], Acc, Count):-
    aux_nSuccessfulParticipants(Rest, Acc, Count).

% Implemente o predicado juriFans(juriFansList), que obtém uma lista contendo, para
% cada participante, a lista dos elementos do júri que não carregaram no botão ao longo
% da sua atuação.

juriFans(JuriFansList):-
    all_Participant(Participants, []),
    aux_juriFans(Participants, [], JuriFansList).


aux_juriFans([], AccList, AccList):-
    !.


aux_juriFans([Participant|Rest], AccList, List):-
    performance(Participant, Times),
    noClickJuris(Times, 1, [], JuriList),
    append(AccList, [Participant-JuriList], AccList2),
    aux_juriFans(Rest, AccList2, List).



noClickJuris([], _, AccList, AccList):-
    !.

noClickJuris([120|Rest], Juri, AccList, List):-
    !,
    append(AccList, [Juri], AccList2),
    NextJuri is Juri +1,
    noClickJuris(Rest, NextJuri, AccList2, List).

noClickJuris([_|Rest], Juri, AccList, List):-
    NextJuri is Juri +1,
    noClickJuris(Rest, NextJuri, AccList, List).

% O seguinte predicado permite obter participantes, suas atuações e tempos totais, que
% estejam em condições de passar à próxima fase: para um participante poder passar,
% tem de haver pelo menos um elemento do júri que não tenha carregado no botão
% durante a sua atuação.

eligibleOutcome(Id,Perf,TT) :-
    performance(Id,Times),
    madeItThrough(Id),
    participant(Id,_,Perf),
    sumlist(Times,TT).


% Fazendo uso deste predicado, implemente o predicado nextPhase(+N, -Participants),
% que obtém a lista com os tempos totais, números e atuações dos N melhores
% participantes, que passarão portanto à próxima fase. Se não houver pelo menos N
% participantes a passar, o predicado deve falhar.

nextPhase(N, Participants):-
    setof(TT-Id-Perf, eligibleOutcome(Id,Perf,TT), List),
    length(List, NFound),
    NFound >= N,
    reverse(List, ReversedList),
    getN(N, 0, ReversedList, [], Participants).


getN(N, N, _, AccList, AccList):-
    !.


getN(N, Index, [Ele|Rest], AccList, List):-
    append(AccList, [Ele], AccList2),
    Index2 is Index + 1,
    getN(N, Index2, Rest, AccList2, List).
    





