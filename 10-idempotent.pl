%
%  10-idempotent.pl
%  marelle-deps
%

% Idempotent packages are actions you can safely execute over and over again.
% A good example would be running sysctl(8) or FreeBSD's sysrc(8).

% execute(+Pkg, +Platform) is semidet.
%   Try to install this package. Same as "meet", but for idempotent packages.
:- multifile execute/2.

:- multifile idempotent_pkg/1.

:- dynamic idempotent_pkg_executed/1.

pkg(Name) :- idempotent_pkg(Name).

met(Name, _) :-
    idempotent_pkg(Name),
    idempotent_pkg_executed(Name).

meet(Name, Platform) :-
    idempotent_pkg(Name), 
    execute(Name, Platform),
    assertz(idempotent_pkg_executed(Name)).
