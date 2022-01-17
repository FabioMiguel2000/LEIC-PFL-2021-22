:- use_module(library(lists)).

% This file contains all functions related to movements of the pieces on the board


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