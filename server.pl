:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_files)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_error)).

:- http_handler(root(.), index, []).
:- http_handler('/uninformed', uninformed, []).

:- [game].


index(Request) :-
    http_reply_file('web/index.html', [], Request).

uninformed(Request) :-
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
    reply_json_dict(_{ lol: Results }).

start() :-
    http_server(http_dispatch, [port(2005)]).

stop() :-
    http_stop_server(2005, []).

:- start().
