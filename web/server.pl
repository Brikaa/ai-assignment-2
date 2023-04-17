:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_files)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_error)).

:- http_handler(root(.), index, []).
:- http_handler('/uninformed', handle_uninformed, []).
:- http_handler('/assets', serve_front_assets, [prefix]).

:- working_directory(_, 'web').

:- ['../engine/game'].

index(Request) :-
    http_reply_file('front/index.html', [], Request).

handle_uninformed(Request) :-
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
    get_game_results(Rows, Columns, bomb1(R1, C1), bomb2(R2, C2), Results),
    reply_json_dict(_{ results: Results }).

serve_front_assets(Request) :-
    member(path(Path), Request),
    atom_concat('front', Path, FilePath),
    exists_file(FilePath),
    !,
    http_reply_file(FilePath, [], Request).

serve_front_assets(Request) :-
    http_404([], Request).

serve() :-
    http_server(http_dispatch, [port(2005)]).

stop() :-
    http_stop_server(2005, []).
