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
Node = {
    state: State,
    parent: Parent,
    g: cumulative cost,
    h: estimated cost till goal,
    f: g + h
}
*/

% --------------------- Generic search algorithm ---------------------

create_node(State, _{state: State, parent: _, g: _, h: _, f: _}).
create_node(State, Parent, _{state: State, parent: Parent, g: _, h: _, f: _}).
create_node(State, Parent, G, H, F, _{state: State, parent: Parent, g: G, h: H, f: F}).

get_solution(State, GoalState, Lists, Acc, Sol) :-
    create_node(State, Parent, Node),
    member(Node, Lists.closed),
    !,
    Accp = [State | Acc],
    get_solution(Parent, GoalState, Lists, Accp, Sol).

get_solution(_, GoalState, _, Acc, Sol) :-
    append(Acc, [GoalState], Sol).

valid_successor_node(ParentState, Lists, AlgoUtil, StateUtil, SuccessorNode) :-
    SuccessorState = SuccessorNode.state,
    \+(ParentState = SuccessorState),
    ValidState = StateUtil.valid,
    call(ValidState, SuccessorState),
    create_node(SuccessorState, ExistingNode),
    ComparePrices = AlgoUtil.compare_node_costs,
    \+((
        member(ExistingNode, Lists.open),
        call(ComparePrices, Delta, ExistingNode, SuccessorNode),
        Delta = <
    )),
    \+member(ExistingNode, Lists.closed).

get_successor_node(ParentNode, Lists, AlgoUtil, StateUtil, SuccessorNode) :-
    ParentState = ParentNode.state,
    create_node(SuccessorState, ParentState, SuccessorG, SuccessorH, SuccessorF, SuccessorNode),
    PerformAction = StateUtil.action,
    CalculateHeuristic = StateUtil.heuristic,
    call(PerformAction, ParentState, SuccessorState, Cost),
    SuccessorG is Cost + ParentNode.g,
    call(CalculateHeuristic, SuccessorState, SuccessorH),
    SuccessorF is SuccessorG + SuccessorH,
    valid_successor_node(ParentState, Lists, AlgoUtil, StateUtil, SuccessorNode).

search(Lists, AlgoUtil, StateUtil, Sol) :-
    Pop = AlgoUtil.pop,
    call(Pop, Lists.open, Node, _),
    GoalTest = StateUtil.goal,
    call(GoalTest, Node.state),
    get_solution(Node.parent, Node.state, Lists, [], Sol).

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

sorted_merge(Pred, [X | Xs], [Y | Ys], [X | Zs]) :-
    call(Pred, <, X, Y),
    !,
    sorted_merge(Pred, Xs, [Y | Ys], Zs).
sorted_merge(Pred, Xs, [Y | Ys], [Y | Zs]) :-
    !,
    sorted_merge(Pred, Xs, Ys, Zs).
sorted_merge(_, [], Ys, Ys) :- !.
sorted_merge(_, Xs, [], Xs).

append_in_order(Pred, Ordered, Unordered, Zs) :-
    predsort(Pred, Unordered, Ordered2),
    sorted_merge(Pred, Ordered, Ordered2, Zs).

a_star_compare_node_costs(>, Node1, Node2) :- Node1.f > Node2.f.
a_star_compare_node_costs(<, Node1, Node2) :- Node1.f < Node2.f.
a_star_compare_node_costs(<, Node1, Node2) :- Node1.f = Node2.f.

% --------------------- Specific search algorithms ---------------------

bfs(InitialState, StateUtil, Sol) :-
    create_node(InitialState, null, 0, 0, 0, InitialNode),
    Lists = _{open: [InitialNode], closed: []},
    AlgoUtil = _{
        pop: pop_first,
        append: append_last,
        compare_node_costs: always_cheaper_node
    },
    search(Lists, AlgoUtil, StateUtil, Sol).

a_star(InitialState, StateUtil, Sol) :-
    create_node(InitialState, null, 0, 0, 0, InitialNode),
    Lists = _{open: [InitialNode], closed: []},
    AlgoUtil = _{
        pop: pop_first,
        append: append_in_order(a_star_compare_node_costs),
        compare_node_costs: a_star_compare_node_costs
    },
    search(Lists, AlgoUtil, StateUtil, Sol).
