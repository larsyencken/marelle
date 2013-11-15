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

test(sformat) :-
    sformat('', [], ''),
    sformat('Ohai', [], 'Ohai'),
    sformat('~a says hello', ['Bob'], 'Bob says hello'),
    sformat('Hello ~a', ['Bob'], 'Hello Bob'),
    sformat('~a, ~a, ~a', ['Once', 'twice', 'three times'],
        'Once, twice, three times'),
    \+ catch(
        sformat('~a and ~a', [romeo], X),
        'wrong number of arguments in interpolation',
        fail
    ).

test(join) :-
    join([], ''),
    join([''], ''),
    join(['one'], 'one'),
    join(['one', ' two'], 'one two'),
    join(['one', ' two', ' and three'], 'one two and three').

:- end_tests(marelle).
