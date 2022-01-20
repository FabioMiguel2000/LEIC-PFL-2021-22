
:- consult('view.pl').
:- consult('board.pl').
:- consult('utils.pl').
:- consult('moves.pl').

:- use_module(library(between)).

:- dynamic(game_over/1).


game_over(false).       % game over flag, change to true when a game over condition is triggered



% main function that begins the game
play:-
    game_board_size(Size),
    initial_state(Size, GameState),
    game_loop(GameState).

% gameloop, ends when game over flag is achieved
game_loop(GameState):-
    game_over(Flag),
    Flag = false,
    !,
    display_game(GameState),
    valid_moves(GameState, ListOfMoves),
    read_user_input(Move, ListOfMoves),
    move(GameState, Move, NewGameState),   
    game_loop(NewGameState).

game_loop(_):-
    write('Game Over!').

% initializes the game state where,
% GameState = [Board, PlayerTurn]
%       -   Board = 2d array representing the board, according with the given Size
%       -   PlayerTurn = in this case white begins
% Size = Size of the game board (Size x Size)
%       -   Size >= 3, but best using 8
initial_state(Size, GameState):-
    Size < 3,
    GameState = error.

initial_state(Size, [GameBoard|PlayerTurn]):-
    Size > 2,
    create_board(Size, GameBoard),
    PlayerTurn = white.

% changes the player turn
change_player_turn(black, white).
change_player_turn(white, black).

% reads from CLI the piece that the user wants to move, the input should be a chess like move 
% (<OldColumn><OldRow><NewColumn><NewRow>, e.g. b1b3, which means peace on B1 moves to B3)
read_user_input(Move, ListOfMoves):-
    repeat,
    read(Input),    %expecting something like d4d5.
    atom_chars(Input, InputList),
    parse_input(InputList, Move, ListOfMoves).

% checks if the input given by user lays between the board range
valid_input_board_range(Col, Row, NewCol, NewRow):-
    game_board_size(BoardSize),
    ColCodeUpperLimit is 97 + BoardSize -1,
    RowCodeUpperLimit is 49 + BoardSize -1,

    char_code(Col, ColCode),
    between(97, ColCodeUpperLimit, ColCode),         % ascci code of 'a' to gameboard size character

    char_code(Row, RowCode),
    between(1, RowCodeUpperLimit, RowCode),         % ascci code of '1' to gameboard size

    char_code(NewCol, NewColCode),
    between(97, ColCodeUpperLimit, NewColCode),         % ascci code of 'a' to gameboard size character

    char_code(NewRow, NewRowCode),
    between(1, RowCodeUpperLimit, NewRowCode).         % ascci code of '1' to gameboard size

% checks if there is actually a move
different_pos(Col, Row, Col, Row):-
    !,
    fail.

different_pos(_,_,_,_).


% parses and validates the input from user, and convert it to [[Row, Col], [NewRow, NewCol]]
parse_input([Col|[Row|[NewCol|[NewRow]]]], Move, ListOfMoves):-
    valid_input_board_range(Col, Row, NewCol, NewRow),      % checks if it is an input inside the board range
    different_pos(Col, Row, NewCol, NewRow),                % not same position
    numberLetter(ColNum, Col),
    numberLetter(NewColNum, NewCol),
    numberLetter(RowNum, Row),
    numberLetter(NewRowNum, NewRow),
    memberchk([[RowNum,ColNum],[NewRowNum,NewColNum]], ListOfMoves),            % Checks it is a valid move
    !,
    Move = [[RowNum,ColNum],[NewRowNum,NewColNum]].


parse_input(_, _,_):-
    log_invalid_move,
    fail.



% move(+GameState, +Move, -NewGameState)
%       -   Move = composed of old postion and new postion
%               - Move = [OldPosition|NewPostion]
%               - Position = [RowNum,ColNum], (e.g. [1,5])
move([GameBoard|PlayerTurn], [[Row,Column],[NewRow,NewColumn]], NewGameState):-
    change_board_element(GameBoard, Row, Column, empty, NewGameBoardTemp),
    change_board_element(NewGameBoardTemp, NewRow, NewColumn, PlayerTurn, NewGameBoardTemp2),
    checkCapture([NewGameBoardTemp2|PlayerTurn], [NewRow|NewColumn], ListOfCaptures),
    removeCapturedPieces(NewGameBoardTemp2, ListOfCaptures, NewGameBoard),
    log_movement_msg(PlayerTurn, Row,Column,NewRow,NewColumn),
    %   MISSING FUNCTION HERE! Check if there is a capture
    change_player_turn(PlayerTurn, NewPlayerTurn),
    game_over(NewGameBoard, NewPlayerTurn),
    NewGameState = [NewGameBoard|NewPlayerTurn].

% change_board_element([[w,w,w,w,w],[e,e,e,e,e],[e,e,e,e,e],[e,e,e,e,e],[b,b,b,b,b]], 1,3,e,R).

% valid_moves(+GameState, -ListOfMoves)
% Returns all valid moves with the game state given (game board and player turn).
valid_moves([GameBoard|PlayerTurn], ListOfMoves):-
    game_board_size(BoardSize),
    element_on_board_moves(GameBoard, BoardSize, PlayerTurn, 1, 1, [], ListOfMoves).
    % length(ListOfMoves, Size),
    % write(Size).


% change_board_element([[e,w,w,w,w],[e,e,e,e,e],[w,e,e,e,e],[e,e,e,e,e],[b,b,b,b,b]], 1,3,e,R).
% valid_horizontal_moves_of_piece([2,2], [[e,w,w,w,w],[w,w,w,e,w],[e,e,e,e,e],[w,e,e,e,e],[e,b,b,b,b]],L).

% valid_vertical_moves_of_piece([5,1], [[w,w,w,w,w],[e,e,e,e,e],[e,e,e,e,e],[e,e,e,e,e],[b,b,b,b,b]],L).

% valid_moves([[[white,white,white,white,white,white,white,white],[empty,empty,empty,empty,empty,empty,empty,empty],[empty,empty,empty,empty,empty,empty,empty,empty],[empty,empty,empty,empty,empty,empty,empty,empty],[empty,empty,empty,empty,empty,empty,empty,empty],[empty,empty,empty,empty,empty,empty,empty,empty],[empty,empty,empty,empty,empty,empty,empty,b],[b,b,b,b,b,b,b,empty]]| b], L).

% check_capture(GameBoard, DestRow, DestColumn, PlayerTurn).

% check_capture_row(GameBoardRow, DestRow, PlayerTurn, ColIndex):-
%     nth1(ColIndex, GameBoardRow, Ele),
%     verify_capture(PlayerTurn, Ele)
    
% verify_capture(PlayerTurn, PlayerTurn). % no cature yet

% verify_capture(PlayerTurn, Ele):-    %There might be a capture
%     \+ PlayerTurn = Ele,
%     % check consecutive Opponents pieces until Own Piece Met


% check_until_own_piece(PlayerTurn, Elem, ColIndex, CapturePositionsList):-
%     %Save current position piece into the list
%     ColIndex2 is ColIndex +1,
%     check_until_own_piece(PlayerTurn, ColIndex2, CapturePositionsList).

% check_until_own_piece(PlayerTurn, PlayerTurn, ColIndex, CapturePositionsList):-
%     %remove all pieces from CapturePositionsList.

checkCapture(GameState, Position, ListOfCaptures):-
    checkMidCapture(GameState, Position, ListOfMidCaptures),        % Finds all captures that happened due to the first condition capture
    checkSurroundCapture(GameState, Position, ListOfSurroundCaptures), % Finds all captures that happened due to the second condition capture
    append(ListOfMidCaptures, ListOfSurroundCaptures, ListOfCaptures).


removeCapturedPieces(GameBoard, [], GameBoard):-
    !.

removeCapturedPieces(GameBoard, [[Row,Col]| Rest], NewGameBoard):-
    change_board_element(GameBoard, Row, Col, empty, TempNewGameBoard),
    removeCapturedPieces(TempNewGameBoard, Rest, NewGameBoard).


checkMidCapture([GameBoard|PlayerTurn], Position, ListOfCaptures):-
    game_board_size(BoardSize),
    checkMidCapture(row, GameBoard, Position, PlayerTurn, BoardSize, ListOfCaptures1),
    checkMidCapture(col, GameBoard, Position, PlayerTurn, BoardSize, ListOfCaptures2),
    append(ListOfCaptures1, ListOfCaptures2, ListOfCaptures).


% check_capture([[[w,b,w,w,b],[b,b,b,w,w],[e,e,e,e,b],[e,e,e,e,e],[b,b,b,b,b]]|w], [2|5], L).

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
    % change_board_element(GameBoard, VerificationRow1, VerificationCol1, empty, TempNewGameBoard),
    % change_board_element(TempNewGameBoard, VerificationRow2, VerificationCol2, empty, NewGameBoard).

aux_checkMidCapture(_, _, _ ,_, []).


% checkSurroundCapture([[[white,white,white,white,white,white,white,white],[empty,empty,empty,empty,empty,empty,empty,black],[empty,white,black,white,black,black,black,white],[empty,empty,empty,empty,empty,empty,empty,empty],[empty,empty,empty,empty,empty,empty,empty,empty],[empty,empty,empty,empty,empty,empty,empty,empty],[empty,empty,empty,empty,empty,empty,empty,b],[b,b,b,b,b,b,b,empty]]| white], [3|4], L).
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

checkSurroundCapture(_, _,_, _, _, _,[]). % if there is an empty space in between, then no capture


game_over(GameBoard, OtherPlayer):- 
    count_list(OtherPlayer, GameBoard, Pawns),      % Checks if game over condition is met
    Pawns < 2,      % If game over condition is achieved
    !,
    retract(game_over(false)),
    asserta(game_over(true)),
    write('You win !').

game_over(_,_). %If not game over

