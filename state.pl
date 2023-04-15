:- [uninformed_search].
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

get_coords_of_horizontal_empty_cells(Matrix, cell1(X1, Y), cell2(X2, Y), Row) :-
    nth0(Y, Matrix, Row),
    nth0(X1, Row, Cell),
    Cell = '#',
    X2 is X1 + 1,
    length(Row, RowLength),
    X2 < RowLength,
    nth0(X2, Row, AdjascentCell),
    AdjascentCell = '#'.

get_coords_of_vertical_empty_cells(Matrix, cell1(X, Y1), cell2(X, Y2), Row, AdjascentRow) :-
    nth0(Y1, Matrix, Row),
    nth0(X, Row, Cell),
    Cell = '#',
    Y2 is Y1 + 1,
    length(Matrix, Height),
    Y2 < Height,
    nth0(Y2, Matrix, AdjascentRow),
    nth0(X, AdjascentRow, AdjascentCell),
    AdjascentCell = '#'.

place_horizontal_domino(Matrix, NewMatrix) :-
    get_coords_of_horizontal_empty_cells(Matrix, cell1(X1, Y), cell2(X2, Y), Row),
    insert_at(X1, 'r', Row, IntermediateRow), % TODO: optimize redundant insert_at if too slow
    insert_at(X2, 'l', IntermediateRow, NewRow),
    insert_at(Y, NewRow, Matrix, NewMatrix).

place_vertical_domino(Matrix, NewMatrix) :-
    get_coords_of_vertical_empty_cells(Matrix, cell1(X, Y1), cell2(X, Y2), Row, AdjascentRow),
    insert_at(X, 'u', Row, NewUpRow),
    insert_at(X, 'd', AdjascentRow, NewDownRow),
    insert_at(Y1, NewUpRow, Matrix, IntermediateMatrix),
    insert_at(Y2, NewDownRow, IntermediateMatrix, NewMatrix).

perform_action(State, NewState) :-
    place_horizontal_domino(State, NewState).
perform_action(State, NewState) :-
    place_vertical_domino(State, NewState).

valid_state(_).

goal_test(State) :-
    findall(X, get_coords_of_horizontal_empty_cells(State, X, _, _), L1),
    L1 = [],
    findall(X, get_coords_of_vertical_empty_cells(State, X, _, _, _), L2),
    L2 = [].

% form_board(3, 4, bomb1(0, 0), bomb2(3, 2), Board), place_horizontal_domino(Board, NewBoard)
% form_board(3, 4, bomb1(0, 0), bomb2(0, 2), Board), place_horizontal_domino(Board, NewBoard)
% form_board(3, 4, bomb1(0, 0), bomb2(0, 2), Board), place_vertical_domino(Board, NewBoard)
% form_board(3, 4, bomb1(0, 0), bomb2(3, 2), Board), place_vertical_domino(Board, NewBoard)
% form_board(3, 4, bomb1(0, 0), bomb2(3, 2), Board), \+goal_test(Board)
% form_board(2, 2, bomb1(0, 0), bomb2(0, 1), Board), place_vertical_domino(Board, NewBoard), goal_test(NewBoard)
/*
form_board(3, 3, bomb1(2, 0), bomb2(0, 1), Board),
bfs(Board, goal_test, perform_action, valid_state, Steps),
last(Steps, Sol).
*/
/*
form_board(2, 4, bomb1(1, 1), bomb2(2, 1), Board),
bfs(Board, goal_test, perform_action, valid_state, Steps),
last(Steps, Sol).
*/

get_final_state(Board, FinalState) :-
    bfs(Board, goal_test, perform_action, valid_state, Sol),
    last(Sol, FinalState).

interactive() :-
    write("Enter the number of rows"),nl,
    read(Rows),nl,
    write("Enter the number of columns"),nl,
    read(Columns),nl,
    write("Enter the X position of bomb 1 (starting at 0)"),
    read(X1),nl,
    write("Enter the Y position of bomb 1 (starting at 0)"),
    read(Y1),nl,
    write("Enter the X position of bomb 2 (starting at 0)"),
    read(X2),nl,
    write("Enter the Y position of bomb 2 (starting at 0)"),
    read(Y2),nl,
    form_board(Rows, Columns, bomb1(X1, Y1), bomb2(X2, Y2), Board),
    findall(S, get_final_state(Board, S), L),
    write(L).
