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

get_empty_cell(Cell) :- Cell = '#'.
get_right_domino_part(D) :- D = r.
get_left_domino_part(D) :- D = l.
get_top_domino_part(D) :- D = u.
get_bottom_domino_part(D) :- D = d.
get_bomb(B) :- B = b.

form_matrix(0, _, []) :- !.
form_matrix(R, C, [X | Xs]) :-
    get_empty_cell(EmptyCell),
    replicate(EmptyCell, C, X),
    R1 is R - 1,
    form_matrix(R1, C, Xs).

place_character(X, Y, Character, InitialMatrix, FinalMatrix) :-
    nth0(Y, InitialMatrix, TargetRow),
    insert_at(X, Character, TargetRow, RowWithBomb),
    insert_at(Y, RowWithBomb, InitialMatrix, FinalMatrix).

form_board(Rows, Columns, bomb1(X1, Y1), bomb2(X2, Y2), Matrix) :-
    form_matrix(Rows, Columns, EmptyMatrix),
    get_bomb(B),
    place_character(X1, Y1, B, EmptyMatrix, MatrixWithFirstBomb),
    place_character(X2, Y2, B, MatrixWithFirstBomb, Matrix).

get_coords_of_horizontal_empty_cells(Matrix, cell1(X1, Y), cell2(X2, Y), Row) :-
    nth0(Y, Matrix, Row),
    nth0(X1, Row, Cell),
    get_empty_cell(Cell),
    X2 is X1 + 1,
    length(Row, RowLength),
    X2 < RowLength,
    nth0(X2, Row, AdjascentCell),
    get_empty_cell(AdjascentCell).

get_coords_of_vertical_empty_cells(Matrix, cell1(X, Y1), cell2(X, Y2), Row, AdjascentRow) :-
    nth0(Y1, Matrix, Row),
    nth0(X, Row, Cell),
    get_empty_cell(Cell),
    Y2 is Y1 + 1,
    length(Matrix, Height),
    Y2 < Height,
    nth0(Y2, Matrix, AdjascentRow),
    nth0(X, AdjascentRow, AdjascentCell),
    get_empty_cell(AdjascentCell).

place_horizontal_domino(Matrix, NewMatrix) :-
    get_coords_of_horizontal_empty_cells(Matrix, cell1(X1, Y), cell2(X2, Y), Row),
    get_right_domino_part(Rd),
    get_left_domino_part(Ld),
    insert_at(X1, Rd, Row, IntermediateRow), % TODO: optimize redundant insert_at if too slow
    insert_at(X2, Ld, IntermediateRow, NewRow),
    insert_at(Y, NewRow, Matrix, NewMatrix).

place_vertical_domino(Matrix, NewMatrix) :-
    get_coords_of_vertical_empty_cells(Matrix, cell1(X, Y1), cell2(X, Y2), Row, AdjascentRow),
    get_top_domino_part(Td),
    get_bottom_domino_part(Bd),
    insert_at(X, Td, Row, NewUpRow),
    insert_at(X, Bd, AdjascentRow, NewDownRow),
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

get_x_y_from_r_c(Row, Column, X, Y) :-
    Y is Row - 1,
    X is Column - 1.

get_game_results(Rows, Columns, bomb1(R1, C1), bomb2(R2, C2), Result) :-
    get_x_y_from_r_c(R1, C1, X1, Y1),
    get_x_y_from_r_c(R2, C2, X2, Y2),
    form_board(Rows, Columns, bomb1(X1, Y1), bomb2(X2, Y2), Board),
    findall(S, get_final_state(Board, S), Result).

% get_game_results(3, 3, bomb1(1, 3), bomb2(2, 1), Result) ; true.

interactive() :-
    write("Enter the number of rows"), nl,
    read(Rows), nl,
    write("Enter the number of columns"), nl,
    read(Columns), nl,
    write("Enter the row of bomb 1"), nl,
    read(R1), nl,
    write("Enter the column of bomb 1"), nl,
    read(C1), nl,
    write("Enter the row of bomb 2"), nl,
    read(R2), nl,
    write("Enter the column of bomb 2"), nl,
    read(C2), nl,
    get_game_results(Rows, Columns, bomb1(R1, C1), bomb2(R2, C2), Result),
    write(Result).
