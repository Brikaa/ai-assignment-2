check_main() :-
    source_file('main.pl'), !.
check_main() :-
    write(
        "The current working directory is not the root directory of the project, please switch to the root directory"
    ), nl,
    write("Halting in 5 seconds"), nl,
    sleep(5),
    halt.

:- check_main().

:- ['web/server'].

:- serve(), www_open_url('http://localhost:2005').
