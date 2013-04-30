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
%  detect(python, _) :- which(python, _).
%  install(python, osx) :- shell('brew install python').
%
:- multifile pkg/1.
:- multifile detect/2.
:- multifile install/2.

% pkg(?Pkg) is nondet.
%   Is this a defined package name?

% detect(+Pkg, +Platform) is semidet.
%   Determine if the package is already installed.

% install(+Pkg, +Platform) is semidet.
%   Try to install this package.


%
%  CORE CODE
%
main :-
    current_prolog_flag(argv, Argv),
    append(Front, Rest, Argv),
    length(Front, 6), !,
    load_deps,
    main(Rest).

main([scan|Tail]) :-
    ( Tail = [] ->
        scan_packages
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
    ( not(pkg(Pkg)) ->
        join(['ERROR: ', Pkg, ' is not defined as a dep'], Msg)
    ; detect(Pkg, Plat) ->
        join(['SUCCESS: ', Pkg, ' is already installed'], Msg)
    ; (install(Pkg, Plat), detect(Pkg, Plat)) ->
        join(['SUCCESS: ', Pkg, ' has been installed'], Msg)
    ;
        join(['FAIL: ', Pkg, ' failed to install'], Msg)
    ),
    writeln(Msg).

main([platform]) :-
    platform(Plat),
    writeln(Plat).

main(_) :- !, usage.

% scan_packages is det.
%   Print all supported packages, marking installed ones with an asterisk.
scan_packages :-
    writeln('Scanning packages...'),
    findall(P, package_state(P), Ps0),
    sort(Ps0, Ps),
    maplist(writepkg, Ps).

package_state(Ann) :-
    platform(Platform),
    pkg(Pkg),
    ground(Pkg),
    ( detect(Pkg, Platform) ->
        Ann = pkg(Pkg, installed)
    ;
        Ann = pkg(Pkg, notinstalled)
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
    writeln('Usage: marelle cmd [args]'),
    writeln(''),
    writeln('Manage package detection and installation. Use "marelle install pkg"'),
    writeln('to install a new package, "marelle installable" to see what\'s available'),
    writeln('or "marelle list" to see what\'s already installed.').

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
    shellc('lsb_release -c | sed \'s/^[^:]*:\\s//g\'', Codename).

writeln_star(L) :- write(L), write(' *\n').

writepkg(pkg(P, installed)) :- writeln_star(P).
writepkg(pkg(P, notinstalled)) :- writeln(P).

install_apt(Name) :-
    join(['sudo apt-get install ', Name], Cmd),
    shell(Cmd, 0).

install_brew(Name) :-
    join(['brew install ', Name], Cmd),
    shell(Cmd, 0).
