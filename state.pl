/*
# # #
b # b

l r #
b # b

# u #
b d b

l, r, u, d are letters representing a part of the domino
If the domino is placed horizontally then r is the right part and l is the left part
If the domino is placed vertically then u is the upper part and d is the lower part

*/

insert_at(0, Element, [_ | Xs], [Element | Xs]) :- !.
insert_at(N, E, [X | Xs], Ys) :-
    Np is N - 1,
    insert_at(Np, E, Xs, Zs),
    Ys = [X | Zs].

replicate(_, 0, []) :- !.
replicate(Char, Number, [Char | List]) :-
    Number1 is Number - 1,
    replicate(Char, Number1, List).

form_matrix(0, _, []) :- !.
form_matrix(R, C, [X | Xs]) :-
    replicate('#', C, X),
    R1 is R - 1,
    form_matrix(R1, C, Xs).

place_character(X, Y, Character, InitialMatrix, FinalMatrix) :-
    nth0(Y, InitialMatrix, TargetRow),
    insert_at(X, Character, TargetRow, RowWithBomb),
    insert_at(Y, RowWithBomb, InitialMatrix, FinalMatrix).

form_board(Rows, Columns, bomb1(X1, Y1), bomb2(X2, Y2), Matrix) :-
    form_matrix(Rows, Columns, EmptyMatrix),
    place_character(X1, Y1, 'b', EmptyMatrix, MatrixWithFirstBomb),
    place_character(X2, Y2, 'b', MatrixWithFirstBomb, Matrix).

place_horizontal_domino(Matrix, NewMatrix) :-
    nth0(CellY, Matrix, Row),
    nth0(CellX, Row, Cell),
    Cell = '#',
    AdjascentCellX is CellX + 1,
    length(Row, RowLength),
    AdjascentCellX < RowLength,
    nth0(AdjascentCellX, Row, NewCell),
    NewCell = '#',
    insert_at(CellX, 'r', Row, IntermediateRow), % TODO: optimize redundant insert_at if too slow
    insert_at(AdjascentCellX, 'l', IntermediateRow, NewRow),
    insert_at(CellY, NewRow, Matrix, NewMatrix).

place_vertical_domino(Matrix, NewMatrix) :-
    nth0(CellY, Matrix, Row),
    nth0(CellX, Row, Cell),
    Cell = '#',
    AdjascentCellY is CellY + 1,
    length(Matrix, Height),
    AdjascentCellY < Height,
    nth0(AdjascentCellY, Matrix, AdjascentRow),
    nth0(CellX, AdjascentRow, AdjascentCell),
    AdjascentCell = '#',
    insert_at(CellX, 'u', Row, NewUpRow),
    insert_at(CellX, 'd', AdjascentRow, NewDownRow),
    insert_at(CellY, NewUpRow, Matrix, IntermediateMatrix),
    insert_at(AdjascentCellY, NewDownRow, IntermediateMatrix, NewMatrix).


% form_board(3, 4, bomb1(0, 0), bomb2(3, 2), Board), place_horizontal_domino(Board, NewBoard)
% form_board(3, 4, bomb1(0, 0), bomb2(0, 2), Board), place_horizontal_domino(Board, NewBoard)
% form_board(3, 4, bomb1(0, 0), bomb2(0, 2), Board), place_vertical_domino(Board, NewBoard)
% form_board(3, 4, bomb1(0, 0), bomb2(3, 2), Board), place_vertical_domino(Board, NewBoard)
