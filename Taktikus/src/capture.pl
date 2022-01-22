:- use_module(library(lists)).
:- reconsult('utils.pl').

% This file contains all functions for capturing of the pieces


% removeCapturedPieces(+GameBoard, +CapturedPiecesList, -NewGameBoard)
% @description: removes all the pieces on the positions contained by the list <CapturedPiecesList> and returns the new game board
removeCapturedPieces(GameBoard, [], GameBoard):-
    !.
    
removeCapturedPieces(GameBoard, [[Row,Col]| Rest], NewGameBoard):-
    change_board_element(GameBoard, Row, Col, empty, TempNewGameBoard),
    removeCapturedPieces(TempNewGameBoard, Rest, NewGameBoard).


% checkCapture(+GameState, +Position, -ListOfCaptures)
% @description: checks all the possible captures that can be done after a move is played to the position <Position>, 
%               the position of the pieces that can be captured is stored in <ListOfCaptures> and returned.
checkCapture(GameState, Position, ListOfCaptures):-
    checkMidCapture(GameState, Position, ListOfMidCaptures),        % Finds all captures that happened due to the first condition capture
    checkSurroundCapture(GameState, Position, ListOfSurroundCaptures), % Finds all captures that happened due to the second condition capture
    append(ListOfMidCaptures, ListOfSurroundCaptures, ListOfCaptures).

% checkMidCapture(+GameState, +Position, -ListOfCaptures)
% @description: checks the possible captures that can be done using the "moving to the middle of opponent's pieces capturing rule" (check README for further details),
%               the position of the pieces that can be captured is stored in <ListOfCaptures> and returned.
checkMidCapture([GameBoard|PlayerTurn], Position, ListOfCaptures):-
    game_board_size(BoardSize),
    checkMidCapture(row, GameBoard, Position, PlayerTurn, BoardSize, ListOfCaptures1),
    checkMidCapture(col, GameBoard, Position, PlayerTurn, BoardSize, ListOfCaptures2),
    append(ListOfCaptures1, ListOfCaptures2, ListOfCaptures).

checkMidCapture(row, _, [_|0], _, _, []):-
    !.
checkMidCapture(row, _, [_|GameBoardSize], _, GameBoardSize, []):-
    !.
checkMidCapture(col, _, [0|_], _, _, []):-
    !.
checkMidCapture(col, _, [GameBoardSize|_], _, GameBoardSize, []):-
    !.

checkMidCapture(row, GameBoard, [PieceRow|PieceCol], PlayerTurn, _, ListOfCaptures):-
    VerificationCol1 is PieceCol +1,
    VerificationCol2 is PieceCol -1,
    aux_checkMidCapture(GameBoard, [PieceRow|VerificationCol1], [PieceRow|VerificationCol2], PlayerTurn, ListOfCaptures).

checkMidCapture(col, GameBoard, [PieceRow|PieceCol], PlayerTurn, _, ListOfCaptures):-
    VerificationRow1 is PieceRow +1,
    VerificationRow2 is PieceRow -1,
    aux_checkMidCapture(GameBoard, [VerificationRow1|PieceCol], [VerificationRow2|PieceCol], PlayerTurn, ListOfCaptures).

% aux_checkMidCapture(+GameBoard, +Position, +PositionIndex, +PlayerTurn, -ListOfCaptures)
% @description: helper function for checkMidCapture(+GameState, +Position, -ListOfCaptures).
aux_checkMidCapture(GameBoard, [VerificationRow1|VerificationCol1], [VerificationRow2|VerificationCol2] ,PlayerTurn, ListOfCaptures):-
    nth1(VerificationRow1, GameBoard, GameBoardRow1),
    nth1(VerificationCol1, GameBoardRow1, Ele1),
    \+ Ele1 = empty,
    \+ Ele1 = PlayerTurn,
    nth1(VerificationRow2, GameBoard, GameBoardRow2),
    nth1(VerificationCol2, GameBoardRow2, Ele2),
    \+ Ele2 = empty,
    \+ Ele2 = PlayerTurn,
    !,  % Condition for this type of capture is met, next, remove Ele1 and Ele2 from the board
    ListOfCaptures = [[VerificationRow1,VerificationCol1], [VerificationRow2,VerificationCol2]].

aux_checkMidCapture(_, _, _ ,_, []).


% checkSurroundCapture([[[white,white,white,white,white,white,white,white],[empty,empty,empty,empty,empty,empty,empty,black],[empty,white,black,white,black,black,black,white],[empty,empty,empty,empty,empty,empty,empty,empty],[empty,empty,empty,empty,empty,empty,empty,empty],[empty,empty,empty,empty,empty,empty,empty,empty],[empty,empty,empty,empty,empty,empty,empty,b],[b,b,b,b,b,b,b,empty]]| white], [3|4], L).
% checkSurroundCapture(+GameState, +PiecePosition, -ListOfCaptures)
% @description: checks the possible captures that can be done using the "surrounding capturing rule" (check README for further details),
%               the position of the pieces that can be captured is stored in <ListOfCaptures> and returned.
checkSurroundCapture([GameBoard|PlayerTurn], PiecePosition, ListOfCaptures):-
    checkSurroundCapture(right, GameBoard, PiecePosition, PiecePosition, PlayerTurn, [], ListOfCapturesRight),
    checkSurroundCapture(left, GameBoard, PiecePosition, PiecePosition, PlayerTurn, [], ListOfCapturesLeft),
    checkSurroundCapture(top, GameBoard, PiecePosition, PiecePosition, PlayerTurn, [], ListOfCapturesTop),
    checkSurroundCapture(bottom, GameBoard, PiecePosition, PiecePosition, PlayerTurn, [], ListOfCapturesBottom),

    append(ListOfCapturesRight, ListOfCapturesLeft, ListOfCapturesHorizontal),
    append(ListOfCapturesTop, ListOfCapturesBottom, ListOfCapturesVertical),
    append(ListOfCapturesHorizontal, ListOfCapturesVertical, ListOfCaptures).


checkSurroundCapture(_, _, _,[0|0], _, _,[]):-
    !.

checkSurroundCapture(DirectionType, GameBoard, [Row|Col],[Row|Col], PlayerTurn, AccList,ListOfCaptures):-
    !,
    index_increment_by_direction(DirectionType, Row, Col, RowIndex2, ColIndex2),
    checkSurroundCapture(DirectionType, GameBoard, [Row|Col], [RowIndex2|ColIndex2], PlayerTurn, AccList,ListOfCaptures).


checkSurroundCapture(DirectionType, GameBoard, [Row|Col], [RowIndex|ColIndex], PlayerTurn, AccList,ListOfCaptures):-
    nth1(RowIndex, GameBoard, GameBoardRow),
    nth1(ColIndex, GameBoardRow, Ele),
    \+ Ele = empty,
    \+ Ele = PlayerTurn,
    !,
    append(AccList, [[RowIndex,ColIndex]], AccList2),
    index_increment_by_direction(DirectionType, RowIndex, ColIndex, RowIndex2, ColIndex2),
    checkSurroundCapture(DirectionType, GameBoard, [Row|Col], [RowIndex2|ColIndex2], PlayerTurn, AccList2,ListOfCaptures).

checkSurroundCapture(_, GameBoard, _, [RowIndex|ColIndex], PlayerTurn, AccList,AccList):-
    nth1(RowIndex, GameBoard, GameBoardRow),
    nth1(ColIndex, GameBoardRow, Ele),
    Ele = PlayerTurn,
    !.

checkSurroundCapture(_, _,_, _, _, _,[]). % if there is an empty space in between, then we can conclude that no capturing will occur