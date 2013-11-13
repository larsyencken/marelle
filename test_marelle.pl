%
%  test_marelle.pl
%  marelle
%
%  Unit tests for Marelle.
%

:- begin_tests(marelle).

:- include('marelle').

test(bash) :-
    bash(echo),
    \+ bash('test 1 -eq 0').

test(isdir) :-
    isdir('.'),
    \+ isdir('7e1b960e8ccf8ed248d05f1803791da7'),
    bash('touch /tmp/7e1b960e8ccf8ed248d05f1803791da7'),
    \+ isdir('/tmp/7e1b960e8ccf8ed248d05f1803791da7'),
    bash('mkdir -p /tmp/2739b22b11ee348c6eda77b57c577485'),
    isdir('/tmp/2739b22b11ee348c6eda77b57c577485').

test(isfile) :-
    isfile('marelle.pl'),
    \+ isfile('.'),
    bash('mkdir -p /tmp/2739b22b11ee348c6eda77b57c577485'),
    \+ isfile('/tmp/2739b22b11ee348c6eda77b57c577485'),
    bash('touch /tmp/7e1b960e8ccf8ed248d05f1803791da7'),
    isfile('/tmp/7e1b960e8ccf8ed248d05f1803791da7').

:- end_tests(marelle).
