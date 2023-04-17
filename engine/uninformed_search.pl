/*
BFS
- Pop a state from the open list
- If the state is goal, return the solution
- For each successor of this state put it in the action list if it is a valid and not repeated state
- Repeat

A*
- Pop a node from the beginning of the open list
- If the node state is a goal state, return the solution
- Expand the node adding its children to the open list in a certain order (according to the algorithm)
    only if the state is valid (not in closed list and not in open list with a less cost)
- Repeat

Lists = {open: list, closed: list}
AlgoUtil = {pop: fn, append: fn}
StateUtil = {goal: fn, action: fn, valid: fn}
search(Lists, AlgoUtil, StateUtil, Sol)
Node = [State, Parent]
*/

get_solution(State, GoalState, Lists, Acc, Sol) :-
    member([State, Parent], Lists.closed),
    !,
    Accp = [State | Acc],
    get_solution(Parent, GoalState, Lists, Accp, Sol).

get_solution(_, GoalState, _, Acc, Sol) :-
    append(Acc, [GoalState], Sol).

valid_successor(State, Lists, StateUtil, Successor) :-
    \+(State = Successor),
    ValidState = StateUtil.valid,
    call(ValidState, Successor),
    \+member([Successor, _], Lists.open),
    \+member([Successor, _], Lists.closed).

get_successor_with_parent(State, Lists, StateUtil, [Successor, State]) :-
    PerformAction = StateUtil.action,
    call(PerformAction, State, Successor),
    valid_successor(State, Lists, StateUtil, Successor).

search(Lists, AlgoUtil, StateUtil, Sol) :-
    Pop = AlgoUtil.pop,
    call(Pop, Lists.open, [State, Parent], _),
    GoalTest = StateUtil.goal,
    call(GoalTest, State),
    get_solution(Parent, State, Lists, [], Sol).

search(Lists, AlgoUtil, StateUtil, Sol) :-
    Pop = AlgoUtil.pop,
    call(Pop, Lists.open, [State, Parent], PoppedOpenList),
    findall(X, get_successor_with_parent(State, Lists, StateUtil, X), Successors),
    Append = AlgoUtil.append,
    call(Append, PoppedOpenList, Successors, NewOpenList),
    NewLists = Lists.put(_{open: NewOpenList, closed: [[State, Parent] | Lists.closed]}),
    search(NewLists, AlgoUtil, StateUtil, Sol).

bfs_append(Xs, Ys, Zs) :- append(Xs, Ys, Zs).
bfs_pop([X | Xs], X, Xs).

bfs(InitialState, StateUtil, Sol) :-
    Lists = _{open: [[InitialState, 'null']], closed: []},
    AlgoUtil = _{pop: bfs_pop, append: bfs_append},
    search(Lists, AlgoUtil, StateUtil, Sol).
