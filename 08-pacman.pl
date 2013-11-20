%
%  08-pacman.pl
%  marelle-deps
%

% installs_with_pacman(Pkg).
%   Pkg installs with apt package of same name on all Ubuntu/Debian flavours
:- multifile installs_with_pacman/1.

% installs_with_pacman(Pkg, PacName).
%   Pkg installs with apt package called AptName on all Ubuntu/Debian
%   flavours. PacName can also be a list of packages.
:- multifile installs_with_pacman/2.

installs_with_pacman(P, P) :- installs_with_pacman(P).

% installs_with_pacman(Pkg, Codename, PacName).
%   Pkg installs with arch linux package called PacName on given Arch Linux
:- multifile installs_with_pacman/3.

installs_with_pacman(P, _, PacName) :- installs_with_pacman(P, PacName).

%depends(P, linux(arch), ['pacman']) :-
%    installs_with_pacman(P, _, _).

:- dynamic pacman_updated/0.

pkg('pacman-update').
met('pacman-update', linux(arch(_))) :- pacman_updated.
meet('pacman-update', linux(arch(_)) :-
    bash('sudo pacman -Syu'),
    assertz(apt_updated).

met(P, linux(Codename)) :-
    installs_with_apt(P, Codename, PkgName). %!,
    %( is_list(PkgName) ->
    %    maplist(check_dpkg, PkgName)
    %;
    %    check_dpkg(PkgName)
    %).

meet(P, linux(Codename)) :-
    installs_with_apt(P, Codename, PkgName).% !,
%( is_list(PkgName) ->
%        maplist(install_apt, PkgName)
%    ;
%        install_apt(PkgName)
%    ).

%check_dpkg(PkgName) :-
%    join(['dpkg -s ', PkgName, ' >/dev/null 2>/dev/null'], Cmd),
%    bash(Cmd).
