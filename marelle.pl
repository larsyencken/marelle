%
%  marelle
%
%  Test driven system administration.
%

%
%  WRITING DEPS
%
%  You need one each of these three statements. E.g.
%
%  pkg(python).
%  met(python, _) :- which(python, _).
%  meet(python, osx) :- shell('brew install python').
%
:- multifile pkg/1.
:- multifile meet/2.
:- multifile met/2.
:- multifile depends/3.

:- dynamic platform/1.

% pkg(?Pkg) is nondet.
%   Is this a defined package name?

% met(+Pkg, +Platform) is semidet.
%   Determine if the package is already installed.

% meet(+Pkg, +Platform) is semidet.
%   Try to install this package.


%
%  CORE CODE
%
%

main :-
    current_prolog_flag(argv, Argv),
    append(Front, Rest, Argv),
    length(Front, 6), !,
    detect_platform,
    load_deps,
    main(Rest).

main([scan|R]) :-
    ( R = ['--all'] ->
        scan_packages(all)
    ; R = ['--missing'] ->
        scan_packages(missing)
    ; R = [] ->
        scan_packages(unprefixed)
    ).

main([status, Pkg]) :-
    ( met(Pkg) ->
        Msg = 'OK'
    ;
        Msg = 'NOT MET'
    ),
    writeln(Msg).

main([meet, Pkg]) :-
    meet_recursive(Pkg).

main([platform]) :-
    platform(Plat),
    writeln(Plat).

main([debug]) :-
    prolog.

main(_) :- !, usage.

meet_recursive(Pkg) :- meet_recursive(Pkg, 0).

meet_recursive(Pkg, Depth0) :-
    ( pkg(Pkg) ->
        join(['PRE-CHECK: ', Pkg], M0),
        writeln_indent(M0, Depth0),
        ( met(Pkg) ->
            join(['SUCCESS: ', Pkg], M1),
            writeln_indent(M1, Depth0)
        ; ( join(['MEETING: ', Pkg], M2),
            writeln_indent(M2, Depth0),
            force_depends(Pkg, Deps),
            exclude(met, Deps, Missing),
            Depth is Depth0 + 1,
            length(Missing, L),
            repeat_val(Depth, L, Depths),
            maplist(meet_recursive, Missing, Depths),
            meet(Pkg),
            join(['POST-CHECK: ', Pkg], M3),
            writeln_indent(M3, Depth0),
            met(Pkg)
        ) ->
            join(['SUCCESS: ', Pkg], M4),
            writeln_indent(M4, Depth0)
        ;
            join(['FAIL: ', Pkg, ' failed to converge'], M5),
            writeln_indent(M5, Depth0)
        )
    ;
        join(['ERROR: ', Pkg, ' is not defined as a dep'], M6),
        writeln_indent(M6, Depth0),
        fail
    ).

repeat_val(X, N, Xs) :-
    repeat_val(X, N, [], Xs).
repeat_val(X, N0, Xs0, Xs) :-
    ( N0 = 0 ->
        Xs = Xs0
    ;
        N is N0 - 1,
        repeat_val(X, N, [X|Xs0], Xs)
    ).


met(Pkg) :-
    platform(P),
    met(Pkg, P).

meet(Pkg) :-
    platform(P),
    meet(Pkg, P).

% force_depends(+Pkg, -Deps) is det.
%   Get a list of dependencies for the given package on this platform. If
%   none exist, return an empty list. Supports multiple matching depends/3
%   statements for a package, or none.
force_depends(Pkg, Deps) :-
    platform(P),
    findall(DepSet, depends(Pkg, P, DepSet), DepSets),
    flatten(DepSets, Deps0),
    list_to_set(Deps0, Deps).

% scan_packages(+Visibility) is det.
%   Print all supported packages, marking installed ones with an asterisk.
scan_packages(Visibility) :-
    writeln('Scanning packages...'),
    findall(P, package_state(P), Ps0),
    sort(Ps0, Ps1),
    ( Visibility = all ->
        Ps = Ps1
    ; Visibility = missing ->
        include(missing, Ps1, Ps)
    ;
        exclude(hidden, Ps1, Ps)
    ),
    maplist(writepkg, Ps).

hidden(pkg(Pkg, _)) :- atom_concat('__', _, Pkg).

missing(pkg(_, notmet)).

package_state(Ann) :-
    platform(Platform),
    pkg(Pkg),
    ground(Pkg),
    ( met(Pkg, Platform) ->
        Ann = pkg(Pkg, met)
    ;
        Ann = pkg(Pkg, notmet)
    ).

% load_deps is det.
%   Looks for dependency files to load from a per-user directory and from
%   a project specific directory.
load_deps :-
    getenv('HOME', Home),
    join([Home, '/.marelle/deps'], PersonalDeps),
    ( exists_directory(PersonalDeps) ->
        load_deps(PersonalDeps)
    ;
        true
    ),
    ( exists_directory('marelle-deps') ->
        load_deps('marelle-deps')
    ;
        true
    ).

load_deps(Dir) :-
    join([Dir, '/*.pl'], Pattern),
    expand_file_name(Pattern, Deps),
    load_files(Deps).

usage :-
    writeln('Usage: marelle scan'),
    writeln('       marelle meet <pkg>'),
    writeln('       marelle platform'),
    writeln(''),
    writeln('Detect and meet dependencies. Searches ~/.marelle/deps and the folder'),
    writeln('marelle-deps in the current directory if it exists.').

% which(+Command, -Path) is semidet.
%   See if a command is available in the current PATH, and return the path to
%   that command.
which(Command, Path) :-
    join(['which ', Command], C),
    shellc(C, Path).

% which(+Command) is semidet.
%   See if a command is available in the current PATH.
which(Command) :- which(Command, _).

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
detect_platform :-
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
    ),
    assertz(platform(Platform)).

join(L, R) :- atomic_list_concat(L, R).

% linux_codename(-Codename).
%   Determine the codename of the linux release (e.g. precise).
linux_codename(Codename) :-
    shellc('lsb_release -c | sed \'s/^[^:]*:\\s//g\'', Codename).

writeln_indent(L, D) :- write_indent(D), writeln(L).
writeln_star(L) :- write(L), write(' *\n').
write_indent(D) :-
    ( D = 0 ->
        true
    ;
        D1 is D - 1,
        write('  '),
        write_indent(D1)
    ).

writepkg(pkg(P, met)) :- writeln_star(P).
writepkg(pkg(P, notmet)) :- writeln(P).

install_apt(Name) :-
    join(['sudo apt-get install ', Name], Cmd),
    shell(Cmd, 0).

install_brew(Name) :-
    join(['brew install ', Name], Cmd),
    shell(Cmd, 0).

home_dir(D0, D) :-
    getenv('HOME', Home),
    join([Home, '/', D0], D).

git_clone(Source, Dest) :-
    join(['git clone --recursive ', Source, ' ', Dest], Cmd),
    shell(Cmd, 0).

%  command packages: met when their command is in path
:- multifile command_pkg/1.
pkg(P) :- command_pkg(P).
met(P, _) :- command_pkg(P), which(P).
