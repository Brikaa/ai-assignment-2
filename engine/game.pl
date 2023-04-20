:- [search].
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

empty_cell(#).
right_domino_part(r).
left_domino_part(l).
top_domino_part(u).
bottom_domino_part(d).
bomb(b).

form_matrix(0, _, []) :- !.
form_matrix(R, C, [X | Xs]) :-
    empty_cell(EmptyCell),
    replicate(EmptyCell, C, X),
    R1 is R - 1,
    form_matrix(R1, C, Xs).

place_character(X, Y, Character, InitialMatrix, FinalMatrix) :-
    nth0(Y, InitialMatrix, TargetRow),
    insert_at(X, Character, TargetRow, RowWithBomb),
    insert_at(Y, RowWithBomb, InitialMatrix, FinalMatrix).

form_board(Rows, Columns, bomb1(X1, Y1), bomb2(X2, Y2), Matrix) :-
    form_matrix(Rows, Columns, EmptyMatrix),
    bomb(B),
    place_character(X1, Y1, B, EmptyMatrix, MatrixWithFirstBomb),
    place_character(X2, Y2, B, MatrixWithFirstBomb, Matrix).

get_coords_of_horizontal_empty_cells(Matrix, cell1(X1, Y), cell2(X2, Y), Row) :-
    nth0(Y, Matrix, Row),
    nth0(X1, Row, Cell),
    empty_cell(Cell),
    X2 is X1 + 1,
    length(Row, RowLength),
    X2 < RowLength,
    nth0(X2, Row, AdjacentCell),
    empty_cell(AdjacentCell).

get_coords_of_vertical_empty_cells(Matrix, cell1(X, Y1), cell2(X, Y2), Row, AdjacentRow) :-
    nth0(Y1, Matrix, Row),
    nth0(X, Row, Cell),
    empty_cell(Cell),
    Y2 is Y1 + 1,
    length(Matrix, Height),
    Y2 < Height,
    nth0(Y2, Matrix, AdjacentRow),
    nth0(X, AdjacentRow, AdjacentCell),
    empty_cell(AdjacentCell).

place_horizontal_domino(Matrix, NewMatrix) :-
    get_coords_of_horizontal_empty_cells(Matrix, cell1(X1, Y), cell2(X2, Y), Row),
    right_domino_part(Rd),
    left_domino_part(Ld),
    insert_at(X1, Rd, Row, IntermediateRow), % TODO: optimize redundant insert_at if too slow
    insert_at(X2, Ld, IntermediateRow, NewRow),
    insert_at(Y, NewRow, Matrix, NewMatrix).

place_vertical_domino(Matrix, NewMatrix) :-
    get_coords_of_vertical_empty_cells(Matrix, cell1(X, Y1), cell2(X, Y2), Row, AdjacentRow),
    top_domino_part(Td),
    bottom_domino_part(Bd),
    insert_at(X, Td, Row, NewUpRow),
    insert_at(X, Bd, AdjacentRow, NewDownRow),
    insert_at(Y1, NewUpRow, Matrix, IntermediateMatrix),
    insert_at(Y2, NewDownRow, IntermediateMatrix, NewMatrix).

insertable_square(Matrix, X, Y) :-
    get_coords_of_horizontal_empty_cells(Matrix, cell1(X, Y), _, _),
    !.

insertable_square(Matrix, X, Y) :-
    get_coords_of_vertical_empty_cells(Matrix, cell1(X, Y), _, _, _).

insertable_squares_heuristic(State, H) :-
    findall(X, insertable_square(State, X, _), L),
    length(L, Ph),
    H is -(Ph).

zero_heuristic(_, 0).

perform_action(State, NewState, (-1)) :-
    place_horizontal_domino(State, NewState).
perform_action(State, NewState, (-1)) :-
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

get_final_state(Board, Algorithm, FinalState) :-
    call(Algorithm, Board, Sol),
    last(Sol, FinalState).

get_x_y_from_r_c(Row, Column, X, Y) :-
    Y is Row - 1,
    X is Column - 1.

bfs(Board, Sol) :-
    bfs(Board, _{goal: goal_test, action: perform_action, valid: valid_state, heuristic: zero_heuristic}, Sol).

a_star(Board, Sol) :-
    a_star(
        Board, _{goal: goal_test, action: perform_action, valid: valid_state, heuristic: insertable_squares_heuristic}, Sol
    ).

all_results_getter(Board, Algorithm, Results) :-
    findall(S, get_final_state(Board, Algorithm, S), Results).

bfs_optimal_result_getter(Board, Algorithm, Results) :-
    findall(S, get_final_state(Board, Algorithm, S), AllResults),
    last(AllResults, OptimalResult),
    Results = [OptimalResult].

a_star_optimal_result_getter(Board, Algorithm, Results) :-
    get_final_state(Board, Algorithm, FinalState),
    !,
    Results = [FinalState].

get_game_results(Rows, Columns, bomb1(R1, C1), bomb2(R2, C2), Algorithm, ResultsGetter, Results) :-
    get_x_y_from_r_c(R1, C1, X1, Y1),
    get_x_y_from_r_c(R2, C2, X2, Y2),
    form_board(Rows, Columns, bomb1(X1, Y1), bomb2(X2, Y2), Board),
    call(ResultsGetter, Board, Algorithm, Results).

% get_game_results(3, 3, bomb1(1, 3), bomb2(2, 1), bfs, all_results_getter, Results) ; true.
% get_game_results(3, 3, bomb1(1, 3), bomb2(2, 1), a_star, all_results_getter, Results) ; true.
% get_game_results(4, 4, bomb1(1, 3), bomb2(2, 1), a_star, a_star_optimal_result_getter, Results) ; true.

count(X, [X | Xs], Acc, Count) :-
    !,
    NewAcc is Acc + 1,
    count(X, Xs, NewAcc, Count).

count(X, [_ | Xs], Acc, Count) :-
    !,
    count(X, Xs, Acc, Count).

count(_, [], Acc, Acc).

count_dominos([Row | Rows], Acc, Count) :-
    !,
    left_domino_part(Ld),
    top_domino_part(Td),
    count(Ld, Row, 0, Nld),
    count(Td, Row, 0, Ntd),
    NewAcc is Nld + Ntd + Acc,
    count_dominos(Rows, NewAcc, Count).

count_dominos([], Acc, Acc).

bfs_max_dominos_getter(Xs, X) :- last(Xs, X).
a_star_max_dominos_getter([X | _], X).

get_max_dominos_from_results(Results, MaxStateGetter, MaxDominos) :-
    call(MaxStateGetter, Results, State),
    count_dominos(State, 0, MaxDominos).

/*
get_game_results(3, 3, bomb1(1, 3), bomb2(2, 1), a_star, Results),
get_max_dominos_from_results(Results, a_star_max_dominos_getter, N).
*/

/*
form_board(3, 5, bomb1(1, 1), bomb2(2, 1), Board),
a_star(Board, _{goal: goal_test, action: perform_action, valid: valid_state, heuristic: insertable_squares_heuristic}, Sol)

form_board(3, 5, bomb1(1, 1), bomb2(2, 1), Board),
bfs(Board, _{goal: goal_test, action: perform_action, valid: valid_state, heuristic: zero_heuristic}, Sol)
*/
