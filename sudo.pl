%
%  sudo.pl
%  marelle
%

% sudo_tell/1.
%   Like tell/1, but works on files which you need to sudo to get privileges
%   for.
sudo_tell(Filename) :-
    expand_path(Filename, ExpFilename),
    join(['cat >', ExpFilename], BashCmd),
    which('sudo', Sudo),
    process_create(
        Sudo,
        ['bash', '-c', BashCmd],
        [stdin(pipe(Stream))]
    ),
    tell(Stream).

% sudo_or_empty/1.
%   Returns an empty string when ran as root, path to sudo with
%   a trailing space otherwise.
sudo_or_empty(Command) :-
    ( bash_output('whoami', root) ->
        Command = ''
    ;
        which('sudo', Sudo),
        join([Sudo, ' '], Command)
    ).
