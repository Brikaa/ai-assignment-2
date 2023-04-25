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
Node = node(State, Parent, G: cumulative cost, H: estimated cost till goal, F: G + H)
*/

% --------------------- Node structure ---------------------
create_node(State, node(State, _, _, _, _)).
create_node(State, Parent, node(State, Parent, _, _, _)).
create_node(State, Parent, G, H, F, node(State, Parent, G, H, F)).

get_state(Node, State) :- arg(1, Node, State).
get_parent(Node, Parent) :- arg(2, Node, Parent).
get_g(Node, G) :- arg(3, Node, G).
get_h(Node, H) :- arg(4, Node, H).
get_f(Node, F) :- arg(5, Node, F).

% --------------------- Generic search algorithm ---------------------

get_solution(State, GoalState, Lists, Acc, Sol) :-
    create_node(State, Parent, Node),
    member(Node, Lists.closed),
    !,
    Accp = [State | Acc],
    get_solution(Parent, GoalState, Lists, Accp, Sol).

get_solution(_, GoalState, _, Acc, Sol) :-
    append(Acc, [GoalState], Sol).

valid_successor_node(ParentState, Lists, AlgoUtil, StateUtil, SuccessorNode) :-
    get_state(SuccessorNode, SuccessorState),
    \+(ParentState = SuccessorState),
    ValidState = StateUtil.valid,
    call(ValidState, SuccessorState),
    create_node(SuccessorState, ExistingNode),
    ComparePrices = AlgoUtil.compare_node_costs,
    \+((
        member(ExistingNode, Lists.open),
        call(ComparePrices, <, ExistingNode, SuccessorNode)
    )),
    \+member(ExistingNode, Lists.closed).

get_successor_node(ParentNode, Lists, AlgoUtil, StateUtil, SuccessorNode) :-
    get_state(ParentNode, ParentState),
    create_node(SuccessorState, ParentState, SuccessorG, SuccessorH, SuccessorF, SuccessorNode),
    PerformAction = StateUtil.action,
    CalculateHeuristic = StateUtil.heuristic,
    call(PerformAction, ParentState, SuccessorState, Cost),
    get_g(ParentNode, ParentG),
    SuccessorG is Cost + ParentG,
    call(CalculateHeuristic, SuccessorState, SuccessorH),
    SuccessorF is SuccessorG + SuccessorH,
    valid_successor_node(ParentState, Lists, AlgoUtil, StateUtil, SuccessorNode).

search(Lists, AlgoUtil, StateUtil, Sol) :-
    Pop = AlgoUtil.pop,
    call(Pop, Lists.open, Node, _),
    GoalTest = StateUtil.goal,
    get_state(Node, State),
    call(GoalTest, State),
    get_parent(Node, Parent),
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

a_star_compare_node_costs(>, Node1, Node2) :-
    get_f(Node1, F1),
    get_f(Node2, F2),
    F1 > F2.
a_star_compare_node_costs(<, Node1, Node2) :-
    get_f(Node1, F1),
    get_f(Node2, F2),
    F1 < F2.
a_star_compare_node_costs(<, Node1, Node2) :-
    get_f(Node1, F1),
    get_f(Node2, F2),
    F1 = F2.

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
