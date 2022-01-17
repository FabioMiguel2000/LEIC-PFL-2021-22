
:- include('view.pl').
:- include('board.pl').
:- include('utils.pl').

:- use_module(library(between)).
:- use_module(library(lists)).

:- dynamic(game_over/1).

game_board_size(8).     % game_board_size(-N), size of the game board, given by N*N, for Taktikus 8x8 is recommended

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
    display_game(GameState),
    read_user_input(Move),
    %   MISSING FUNCTION HERE! Check if it is a valid move, if so then let it move
    move(GameState, Move, NewGameState),
    game_loop(NewGameState).

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
read_user_input(Move):-
    repeat,
    read(Input),    %expecting something like d4d5.
    atom_chars(Input, InputList),
    parse_input(InputList, Move).

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
parse_input([Col|[Row|[NewCol|[NewRow]]]], Move):-
    valid_input_board_range(Col, Row, NewCol, NewRow),
    different_pos(Col, Row, NewCol, NewRow),
    !,
    numberLetter(ColNum, Col),
    numberLetter(NewColNum, NewCol),
    numberLetter(RowNum, Row),
    numberLetter(NewRowNum, NewRow),
    Move = [[RowNum,ColNum],[NewRowNum,NewColNum]].

parse_input(_, _):-
    write('Invalid Move! Please input again!\n'),
    fail.



% move(+GameState, +Move, -NewGameState)
%       -   Move = composed of old postion and new postion
%               - Move = [OldPosition|NewPostion]
%               - Position = [RowNum,ColNum], (e.g. [1,5])
move([GameBoard|PlayerTurn], [[Row,Column],[NewRow,NewColumn]], NewGameState):-
    change_board_element(GameBoard, Row, Column, empty, NewGameBoardTemp),
    change_board_element(NewGameBoardTemp, NewRow, NewColumn, PlayerTurn, NewGameBoard),
    %   MISSING FUNCTION HERE! Check if there is a capture
    change_player_turn(PlayerTurn, NewPlayerTurn),
    NewGameState = [NewGameBoard|NewPlayerTurn].


% valid_moves(+GameState, -ListOfMoves)
% Returns all valid moves with the game state given (game board and player turn).
valid_moves([GameBoard|[PlayerTurn]], ListOfMoves):-
    game_board_size(BoardSize),
    element_on_board_moves(GameBoard, BoardSize, PlayerTurn, 1, 1, [], ListOfMoves).

% index_increment(+BoardSize, +RowIndex, +ColIndex, -RowIndex2, -ColIndex2)
% Updates the index to loop through the gameboard, on finish <RowIndex2> and <ColIndex2> are set to 0.
index_increment(BoardSize, BoardSize, BoardSize, 0, 0):- % Ends
    !.
index_increment(BoardSize, RowIndex, BoardSize, RowIndex2, 1):-
    !,
    RowIndex2 is RowIndex + 1.

index_increment(BoardSize, RowIndex, ColIndex, RowIndex2, ColIndex2):-
    ColIndex < BoardSize,
    ColIndex2 is ColIndex + 1,
    RowIndex2 = RowIndex.

% element_on_board_moves(+GameBoard, +BoardSize, +PlayerTurn, +RowIndex, +ColIndex, +AccList, -ListOfMoves)
% Loops each piece on the board, and finds out all the possible moves by the pieces of the current player.
element_on_board_moves(_, _, _, 0, 0, AccList, AccList):-
    !.

element_on_board_moves(GameBoard, BoardSize, PlayerTurn, RowIndex, ColIndex, AccList, ListOfMoves):-
    nth1(RowIndex, GameBoard, BoardRow),
    nth1(ColIndex, BoardRow, Elem),
    Elem = PlayerTurn,
    !,
    valid_moves_by_piece([RowIndex, ColIndex], GameBoard, MovesByPiece),
    append(AccList, MovesByPiece, AccList2),
    index_increment(BoardSize, RowIndex, ColIndex, RowIndex2, ColIndex2),
    element_on_board_moves(GameBoard, BoardSize, PlayerTurn, RowIndex2, ColIndex2, AccList2,ListOfMoves).

element_on_board_moves(GameBoard, BoardSize, PlayerTurn, RowIndex, ColIndex, AccList, ListOfMoves):-
    index_increment(BoardSize, RowIndex, ColIndex, RowIndex2, ColIndex2),
    element_on_board_moves(GameBoard, BoardSize, PlayerTurn, RowIndex2, ColIndex2, AccList,ListOfMoves).

% valid_moves_by_piece(+PiecePosition, +GameBoard, -ListOfMoves)
% For a given piece on the board, finds all the possible moves that it can take
valid_moves_by_piece(PiecePosition, GameBoard, ListOfMoves):-
    aux_valid_moves_by_piece(top,PiecePosition, GameBoard, TopMovesByPiece),            % Finds all moves available to the top
    aux_valid_moves_by_piece(bottom,PiecePosition, GameBoard, BotMovesByPiece),         % Finds all moves available to the bottom
    append(TopMovesByPiece, BotMovesByPiece, VerticalMovesByPiece),     

    aux_valid_moves_by_piece(left,PiecePosition, GameBoard, LeftMovesByPiece),          % Finds all moves available to the left
    aux_valid_moves_by_piece(right,PiecePosition, GameBoard, RightMovesByPiece),        % Finds all moves available to the right
    append(LeftMovesByPiece, RightMovesByPiece, HorizontalMovesByPiece),

    append(HorizontalMovesByPiece, VerticalMovesByPiece, ListOfMoves).

% aux_valid_moves_by_piece(+DirectionType, +PiecePosition, +GameBoard, -ListOfMoves)
% For a given piece on the board, finds all the possible moves for a particular direction (top, bottom, right or left).
aux_valid_moves_by_piece(DirectionType,[Row,Col], GameBoard, ListOfMoves):-
    aux_valid_moves_by_piece(DirectionType,[Row,Col], Row, Col, GameBoard, [], ListOfMoves).

aux_valid_moves_by_piece(_,_,0,_,_, AccumulatorList, AccumulatorList):-
    !.

aux_valid_moves_by_piece(_,_,_,0,_, AccumulatorList, AccumulatorList):-
    !.

aux_valid_moves_by_piece(DirectionType,[RowIndex,ColIndex],RowIndex, ColIndex,GameBoard, AccumulatorList, ListOfMoves):-
    !,
    index_increment_by_direction(DirectionType, RowIndex, ColIndex, RowIndex2, ColIndex2),
    aux_valid_moves_by_piece(DirectionType,[RowIndex,ColIndex],RowIndex2, ColIndex2,GameBoard,AccumulatorList, ListOfMoves).

aux_valid_moves_by_piece(DirectionType,[Row,Col],RowIndex, ColIndex,GameBoard, AccumulatorList, ListOfMoves):-
    nth1(RowIndex, GameBoard, BoardRow),
    nth1(ColIndex, BoardRow, Elem),
    Elem = empty,
    append(AccumulatorList, [[[Row,Col], [RowIndex, ColIndex]]], AccumulatorList2),
    index_increment_by_direction(DirectionType, RowIndex, ColIndex, RowIndex2, ColIndex2),
    aux_valid_moves_by_piece(DirectionType,[Row,Col],RowIndex2, ColIndex2,GameBoard,AccumulatorList2, ListOfMoves).

aux_valid_moves_by_piece(_,_,_,_,_, AccumulatorList, AccumulatorList).

% index_increment_by_direction(+DirectionType, +RowIndex, +ColIndex, -RowIndex2, ColIndex2)
% Depending on the type of direction, increments the index to loop through the board.
index_increment_by_direction(top, RowIndex, ColIndex, RowIndex2, ColIndex):-
    !,RowIndex2 is RowIndex - 1.

index_increment_by_direction(bottom, RowIndex, ColIndex, RowIndex2, ColIndex):-
    !,RowIndex2 is RowIndex + 1.

index_increment_by_direction(right, RowIndex, ColIndex, RowIndex, ColIndex2):-
    !,ColIndex2 is ColIndex + 1.

index_increment_by_direction(left, RowIndex, ColIndex, RowIndex, ColIndex2):-
    !,ColIndex2 is ColIndex - 1.

% change_board_element([[e,w,w,w,w],[e,e,e,e,e],[w,e,e,e,e],[e,e,e,e,e],[b,b,b,b,b]], 1,3,e,R).
% valid_horizontal_moves_of_piece([2,2], [[e,w,w,w,w],[w,w,w,e,w],[e,e,e,e,e],[w,e,e,e,e],[e,b,b,b,b]],L).

% valid_vertical_moves_of_piece([5,1], [[w,w,w,w,w],[e,e,e,e,e],[e,e,e,e,e],[e,e,e,e,e],[b,b,b,b,b]],L).

% valid_moves([[[w,w,w,w,w],[e,e,e,e,e],[e,e,e,e,e],[b,e,e,e,e],[e,b,b,b,b]], b], L).