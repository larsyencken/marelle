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
    ; R = [] ->
        scan_packages(some)
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

main(_) :- !, usage.

meet_recursive(Pkg) :-
    ( not(pkg(Pkg)) ->
        join(['ERROR: ', Pkg, ' is not defined as a dep'], Msg)
    ; met(Pkg) ->
        join(['SUCCESS: ', Pkg], Msg)
    ; ( join(['MEETING: ', Pkg], Msg0),
        writeln(Msg0),
        force_depends(Pkg, Deps),
        exclude(met, Deps, Missing),
        maplist(meet_recursive, Missing),
        meet(Pkg),
        writeln('ok here'),
        met(Pkg)
    ) ->
        join(['SUCCESS: ', Pkg], Msg)
    ;
        join(['FAIL: ', Pkg, ' failed to converge'], Msg)
    ),
    writeln(Msg).

met(Pkg) :-
    join(['CHECKING: ', Pkg], Msg),
    writeln(Msg),
    platform(P),
    met(Pkg, P).

meet(Pkg) :-
    platform(P),
    meet(Pkg, P).

% force_depends(+Pkg, -Deps) is det.
%   Get a list of dependencies for the given package on this platform. If
%   none exist, return an empty list.
force_depends(Pkg, Deps) :-
    platform(P),
    ( depends(Pkg, P, Deps) ->
        true
    ;
        Deps = []
    ).

% scan_packages(+Visibility) is det.
%   Print all supported packages, marking installed ones with an asterisk.
scan_packages(Visibility) :-
    writeln('Scanning packages...'),
    findall(P, package_state(P), Ps0),
    sort(Ps0, Ps1),
    ( Visibility = all ->
        Ps = Ps1
    ;
        exclude(hidden, Ps1, Ps)
    ),
    maplist(writepkg, Ps).

hidden(pkg(Pkg, _)) :- atom_concat('__', _, Pkg).

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
    writeln('       marelle install <pkg>'),
    writeln('       marelle platform'),
    writeln(''),
    writeln('Detect and install packages. Searches ~/.marelle/deps and the folder'),
    writeln('marelle-deps in the current directory if it exists.').

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

writeln_star(L) :- write(L), write(' *\n').

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
    join(['git clone ', Source, ' ', Dest], Cmd),
    shell(Cmd, 0).
