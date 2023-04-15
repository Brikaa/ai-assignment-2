/*
# # #
b # b

r l #
b # b

# d #
b u b

board(Matrix, bomb1(x, y), bomb2(x, y))
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

place_bomb(X, Y, InitialMatrix, FinalMatrix) :-
    nth0(Y, InitialMatrix, TargetRow),
    insert_at(X, 'b', TargetRow, RowWithBomb),
    insert_at(Y, RowWithBomb, InitialMatrix, FinalMatrix).

form_board(Rows, Columns, bomb1(X1, Y1), bomb2(X2, Y2), board(Matrix, bomb1(X1, Y1), bomb2(X2, Y2))) :-
    form_matrix(Rows, Columns, EmptyMatrix),
    place_bomb(X1, Y1, EmptyMatrix, MatrixWithFirstBomb),
    place_bomb(X2, Y2, MatrixWithFirstBomb, Matrix).

% form_board(3, 4, bomb1(0, 0), bomb2(2, 3), Board)
