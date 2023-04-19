:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_files)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_error)).

:- http_handler(root(.), index, []).
:- http_handler('/bfs', handle_bfs, []).
:- http_handler('/assets', serve_front_assets, [prefix]).

:- ['engine/game'].

index(Request) :-
    http_reply_file('web/front/index.html', [], Request).

handle_bfs(Request) :-
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
    get_game_results(Rows, Columns, bomb1(R1, C1), bomb2(R2, C2), bfs, Results),
    !,
    reply_json_dict(_{ results: Results }).

handle_bfs(_) :-
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
