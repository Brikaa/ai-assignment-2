:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_files)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_error)).

:- http_handler(root(.), index, []).
:- http_handler('/bfs/all', handle_game_request(bfs, all_results_getter, bfs_max_dominos_getter), []).
:- http_handler('/a_star/all', handle_game_request(a_star, all_results_getter, a_star_max_dominos_getter), []).
:- http_handler('/bfs/optimal', handle_game_request(bfs, bfs_optimal_result_getter, bfs_max_dominos_getter), []).
:- http_handler(
    '/a_star/optimal', handle_game_request(a_star, a_star_optimal_result_getter, a_star_max_dominos_getter), []
).
:- http_handler('/assets', serve_front_assets, [prefix]).

:- ['engine/game'].

index(Request) :-
    http_reply_file('web/front/index.html', [], Request).

response(Results, MaxDominos, _{ results: Results, maxDominos: MaxDominos }).

handle_game_request(Algorithm, ResultsGetter, MaxStateGetter, Request) :-
    /*
    rows: number,
    columns: number,
    bomb1: { row: number, column: number },
    bomb2: { row: number, column: number }
    */
    http_read_json_dict(Request, _{
        rows: Rows,
        columns: Columns,
        bomb1: _{ row: R1, column: C1 },
        bomb2: _{ row: R2, column: C2 }
    }),
    get_game_results(Rows, Columns, bomb1(R1, C1), bomb2(R2, C2), Algorithm, ResultsGetter, Results),
    !,
    get_max_dominos_from_results(Results, MaxStateGetter, MaxDominos),
    response(Results, MaxDominos, Response),
    reply_json_dict(Response).

handle_game_request(_, _, _, _) :-
    reply_json_dict(_{ error: "Invalid inputs" }, [status(400)]).

serve_front_assets(Request) :-
    member(path(Path), Request),
    atom_concat('web/front', Path, FilePath),
    exists_file(FilePath),
    !,
    http_reply_file(FilePath, [], Request).

serve_front_assets(Request) :-
    http_404([], Request).

port(2005).

serve() :-
    port(Port),
    http_server(http_dispatch, [port(Port)]).

stop() :-
    port(Port),
    http_stop_server(Port, []).
