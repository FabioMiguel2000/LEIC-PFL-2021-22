%airport (Name, ICAO, Country).
airport('Aeroporto Francisco Sá Carneiro', 'LPPR', 'Portugal').
airport('Aeroporto Humberto Delgado','LPPT', 'Portugal').
airport('Neropuerto Adolfo Suárez Madrid-Barajas', 'LEMD', 'Spain').
airport('Aéroport de Paris-Charles-de-Gaulle Ro1ssy Airport', 'LFPG', 'France').
airport('Aeroporto Internazionale di Roma-F1umicino - Leonardo da Vinci','LIRF', 'Italy').

%company (ICAO, Name, Year, Country)
company('TAP','TAP Air Portugal', 1945, 'Portugal').
company('RYR','Ryanair', 1984, 'Ireland').
company('AFR' , 'Société Air France, S.A.', 1933, 'France').
company('BAW','British Airways', 1974,'United Kingdom').


%flight (Designation, Origin, Destination, DepartureTime, Duration, Company) ,
flight('TP1923', 'LPPR','LPPT', 1115, 55, 'TAP').
flight('TP1968', 'LPPT', 'LPPR', 2235, 55, 'TAP').
flight('TP842','LPPT','LIRF',1450, 195, 'TAP').
flight('TP843','LIRF', 'LPPT', 1935,195,'TAP').
flight('FR5483','LPPR','LEMD', 630, 105,'RYR').
flight('FR5484','LEMD','LPPR', 1935, 105, 'RYR').
flight('AF1024', 'LEPG', 'IPPT', 940, 155, 'AFR').
flight('AF1025','LPPT', 'LFPG', 1310, 155,'AFR').


% Pergunta 1

short(Flight):-
    flight(Flight, _Origin, _Dest, _DepartTime, Duration, _Company),
    !,
    Duration < 90.

% Pergunta 2

shorter(Flight1, Flight2, ShorterFlight):-
    flight(Flight1, _Origin1, _Dest1, _DepartTime1, Duration1, _Company1),
    !,
    flight(Flight2, _Origin2, _Dest2, _DepartTime2, Duration2, _Company2),
    !,
    shorter_flight(Flight1, Flight2, Duration1, Duration2, ShorterFlight).
    
shorter_flight(Flight1, _Flight2,Val1, Val2, Flight1):-
    Val1 < Val2,
    !.

shorter_flight(_Flight1, Flight2,Val1, Val2, Flight2):-
    Val2 < Val1,
    !.

shorter_flight(_Flight1, _Flight2,_Val1, _Val2, _Flight1):-
    fail.

% Pergunta 3

arrivalTime(Flight, ArrivalTime):-
    flight(Flight, _Origin, _Dest, DepartureTime, Duration, _Company),
    addTime(Time1, Time2, Result)

addTime(Time1, Minutes2, Result):-
    Minutes1 is Time1 mod 100,
    Minutes is (Minutes1 + Minutes2) mod 60,
    AccHour is (Minutes1 + Minutes2) div 60,
    Hour1 is Time1 div 100,
    Hour is (Hour1 + AccHour) mod 24,
    Result = Hour*100 + Minutes.
