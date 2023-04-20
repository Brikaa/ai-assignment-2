/*
A generic search algorithm is used.
Procedures from the specific search algorithms are plugged in (dependency inversion)

BFS
- Pop a state from the open list
- If the state is goal, return the solution
- For each successor of this state put it in the action list if it is a valid and not repeated state
- Repeat

A*
- Pop a node from the beginning of the open list
- If the node state is a goal state, return the solution
- Expand the node adding its children to the open list in a certain order (h(x) + f(x) > h(y) + f(y))
    only if the state is valid (not in closed list and not in open list with a less cost)
- Repeat

Lists = {open: list, closed: list}
AlgoUtil = {pop: fn, append: fn, compare_node_costs: fn(Delta, X, Y)}
StateUtil = {goal: fn, action: fn, valid: fn, heuristic: fn}
search(Lists, AlgoUtil, StateUtil, Sol)
Node = [State, Parent, g, h, f]
*/

% --------------------- Generic search algorithm ---------------------

get_solution(State, GoalState, Lists, Acc, Sol) :-
    member([State, Parent | _], Lists.closed),
    !,
    Accp = [State | Acc],
    get_solution(Parent, GoalState, Lists, Accp, Sol).

get_solution(_, GoalState, _, Acc, Sol) :-
    append(Acc, [GoalState], Sol).

valid_successor(ParentNode, Lists, AlgoUtil, StateUtil, SuccessorNode) :-
    [ParentState | _] = ParentNode,
    [SuccessorState | _] = SuccessorNode,
    \+(ParentState = SuccessorState),
    ValidState = StateUtil.valid,
    call(ValidState, SuccessorState),
    ExistingNode = [SuccessorState | _],
    ComparePrices = AlgoUtil.compare_node_costs,
    \+((
        member(ExistingNode, Lists.open),
        call(ComparePrices, Delta, ExistingNode, SuccessorNode),
        Delta = <
    )),
    \+member(ExistingNode, Lists.closed).

get_successor_node(ParentNode, Lists, AlgoUtil, StateUtil, SuccessorNode) :-
    [ParentState, _Parent, G | _] = ParentNode,
    SuccessorNode = [SuccessorState, ParentState, SuccessorG, SuccessorH, SuccessorF],
    PerformAction = StateUtil.action,
    CalculateHeuristic = StateUtil.heuristic,
    call(PerformAction, ParentState, SuccessorState, Cost),
    SuccessorG is Cost + G,
    call(CalculateHeuristic, SuccessorState, SuccessorH),
    SuccessorF is SuccessorG + SuccessorH,
    valid_successor(ParentNode, Lists, AlgoUtil, StateUtil, SuccessorNode).

search(Lists, AlgoUtil, StateUtil, Sol) :-
    Pop = AlgoUtil.pop,
    call(Pop, Lists.open, [State, Parent | _], _),
    GoalTest = StateUtil.goal,
    call(GoalTest, State),
    get_solution(Parent, State, Lists, [], Sol).

search(Lists, AlgoUtil, StateUtil, Sol) :-
    Pop = AlgoUtil.pop,
    call(Pop, Lists.open, Node, PoppedOpenList),
    findall(X, get_successor_node(Node, Lists, AlgoUtil, StateUtil, X), Successors),
    Append = AlgoUtil.append,
    call(Append, PoppedOpenList, Successors, NewOpenList),
    NewClosedList = [Node | Lists.closed],
    NewLists = Lists.put(_{open: NewOpenList, closed: NewClosedList}),
    search(NewLists, AlgoUtil, StateUtil, Sol).

% --------------------- Predicates for the specific search algorithms (abstraction applications) ---------------------

append_last(Xs, Ys, Zs) :- append(Xs, Ys, Zs).
pop_first([X | Xs], X, Xs).
always_cheaper_node(<, _, _).

append_in_order(Pred, Xs, Ys, Zs) :-
    append(Xs, Ys, Ws),
    predsort(Pred, Ws, Zs).

a_star_compare_node_costs('>', [_, _, _, _, F1], [_, _, _, _, F2]) :- F1 > F2.
a_star_compare_node_costs('<', [_, _, _, _, F1], [_, _, _, _, F2]) :- F1 < F2.
a_star_compare_node_costs('<', [_, _, _, _, F1], [_, _, _, _, F2]) :- F1 = F2.

% --------------------- Specific search algorithms ---------------------

bfs(InitialState, StateUtil, Sol) :-
    InitialNode = [InitialState, [], 0, 0, 0],
    Lists = _{open: [InitialNode], closed: []},
    AlgoUtil = _{
        pop: pop_first,
        append: append_last,
        compare_node_costs: always_cheaper_node
    },
    search(Lists, AlgoUtil, StateUtil, Sol).

a_star(InitialState, StateUtil, Sol) :-
    InitialNode = [InitialState, [], 0, 0, 0],
    Lists = _{open: [InitialNode], closed: []},
    AlgoUtil = _{
        pop: pop_first,
        append: append_in_order(a_star_compare_node_costs),
        compare_node_costs: a_star_compare_node_costs
    },
    search(Lists, AlgoUtil, StateUtil, Sol).
