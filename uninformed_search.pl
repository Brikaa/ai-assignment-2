get_solution(State, GoalState, ClosedList, Acc, Sol) :-
    member([Parent, State], ClosedList),
    !,
    Accp = [State | Acc],
    get_solution(Parent, GoalState, ClosedList, Accp, Sol).

get_solution(_, GoalState, _, Acc, Sol) :-
    append(Acc, [GoalState], Sol).

valid_successor(State, OpenList, ClosedList, ValidState, Successor) :-
    \+(State = Successor),
    call(ValidState, Successor),
    \+member([_, Successor], OpenList),
    \+member([_, Successor], ClosedList).

get_successor_with_parent(State, OpenList, ClosedList, ValidState, PerformAction, [State, Successor]) :-
    call(PerformAction, State, Successor),
    valid_successor(State, OpenList, ClosedList, ValidState, Successor).

search(OpenList, ClosedList, Pop, _, GoalTest, _, _, Sol) :-
    call(Pop, OpenList, [Parent, State], _),
    call(GoalTest, State), !,
    get_solution(Parent, State, ClosedList, [], Sol).

search(OpenList, ClosedList, Pop, Append, GoalTest, PerformAction, ValidState, Sol) :-
    call(Pop, OpenList, [Parent, State], PoppedOpenList),
    findall(X, get_successor_with_parent(State, OpenList, ClosedList, ValidState, PerformAction, X), Successors),
    call(Append, PoppedOpenList, Successors, NewOpenList),
    search(NewOpenList, [[Parent, State] | ClosedList], Pop, Append, GoalTest, PerformAction, ValidState, Sol).

bfs_append(Xs, Ys, Zs) :- append(Xs, Ys, Zs).
bfs_pop([X | Xs], X, Xs).

bfs(InitialState, GoalTest, PerformAction, ValidState, Sol) :-
    search([[[], InitialState]], [], bfs_pop, bfs_append, GoalTest, PerformAction, ValidState, Sol).
