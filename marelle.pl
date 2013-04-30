%
%  marelle
%
%  Test driven system administration.
%

main :-
    current_prolog_flag(argv, Argv),
    append(Front, Rest, Argv),
    length(Front, 6), !,
    load_deps,
    main(Rest).

main([installed|Tail]) :-
    ( Tail = [] ->
        platform(P),
        findall(X, detect(X, P), Xs),
        maplist(writeln, Xs)
    ; Tail = [Pkg] ->
        platform(P),
        ( detect(Pkg, P) ->
            join([Pkg, ' is installed'], Msg)
        ;
            join([Pkg, ' is not installed'], Msg)
        ),
        writeln(Msg)
    ).

main([install, Pkg]) :-
    platform(Plat),
    ( not(has_detect_rule(Pkg, Plat)) ->
        join([Pkg, ' has no rule to detect it on ', Plat], Msg)
    ; detect(Pkg, Plat) ->
        join([Pkg, ' is already installed'], Msg)
    ; not(has_install_rule(Pkg, Plat)) ->
        join([Pkg, ' has no install rule on ', Plat], Msg)
    ; (install(Pkg, Plat), detect(Pkg, Plat)) ->
        join([Pkg, ' has been installed'], Msg)
    ;
        join([Pkg, ' failed to install'], Msg)
    ),
    writeln(Msg).

main([listable]) :-
    platform(Plat),
    findall(X, has_detect_rule(X, Plat), Xs),
    maplist(writeln, Xs).

main([list]) :-
    platform(Plat),
    findall(X, has_install_rule(X, Plat), Xs),
    maplist(writeln, Xs).

main([platform]) :-
    platform(Plat),
    writeln(Plat).

main(_) :- !, usage.

load_deps :-
    expand_file_name('marelle-deps/*.pl', Files),
    load_files(Files).

usage :-
    writeln('Usage: marelle cmd [args]'),
    writeln(''),
    writeln('Manage package detection and installation. Use "marelle install pkg"'),
    writeln('to install a new package, "marelle installable" to see what\'s available'),
    writeln('or "marelle list" to see what\'s already installed.').

has_detect_rule(Pkg, Platform) :-
    Term =.. [detect, Pkg, Platform],
    callable(Term).

has_install_rule(Pkg, Platform) :-
    Term =.. [install, Pkg, Platform],
    callable(Term).

:- multifile detect/2.

% detect(?Pkg, ?Platform).
%   Determine if the package is already installed.
detect('python', _) :- which('python', _).
detect('python2.7', _) :- which('python2.7', _).
detect('ruby', _) :- which('ruby', _).
detect('racket', _) :- which('racket', _).

:- multifile install/2.

install('racket', 'osx') :- shellc('brew install plt-racket').


% which(+Command, -Path).
%   See if a command is available in the current path.
which(Command, Path) :-
    join(['which ', Command], C),
    shellc(C, Path).

% shellc(+Cmd, -Output).
%   Execute the command in a shellc, and fetch the output as an atom.
shellc(Cmd, Output) :-
    tmp_file(syscmd, TmpFile),
    join([Cmd, ' >', TmpFile], Call),
    shell(Call, 0),
    read_file_to_codes(TmpFile, Codes, []),
    atom_codes(Raw, Codes),
    atom_concat(Output, '\n', Raw).

% platform(-Platform).
%   Determines the current platform (e.g. osx, ubuntu).
platform(Platform) :-
    shellc('uname -s', Result),
    ( Result = 'Linux' ->
        ( which('lsb_release', _) ->
            linux_codename(Codename),
            Platform = linux(Codename)
        ;
            Platform = linux(unknown)
        )
    ; Result = 'Darwin' ->
        Platform = osx
    ;
        Platform = unknown
    ).

join(L, R) :- atomic_list_concat(L, R).

% linux_codename(-Codename).
%   Determine the codename of the linux release (e.g. precise).
linux_codename(Codename) :-
    shellc('lsb_release -c | sed \'s/^[^:]*:\s//g\'', Codename).
