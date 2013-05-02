# Marelle ("hopscotch")

Test-driven system administration in SWI-Prolog, in the style of [Babushka](https://github.com/babushka/babushka).

Marelle uses [logic programming](https://en.wikipedia.org/wiki/Logic_programming), one of the four main programming paradigms, to describe system targets and rules by which these targets can be met. Prolog's built-in search mechanism makes writing and using these dependencies elegant. Anecdotally, writing deps for Marelle has the feel of teaching it about types of packages, rather than the feel of writing package templates.

## Current status

Pre-alpha, but stabilizing.

![Hopscotch for Seniors](https://bytebucket.org/larsyencken/marelle/wiki/img/HopscotchForSeniors.jpg)

## Installing marelle

### Quickstart

Run the bootstrap script:

```bash
bash -c "`curl https://bitbucket.org/larsyencken/marelle/raw/master/bootstrap.sh`"
```

This will install marelle as the current user, putting the executable in `~/.local/bin/marelle`.

### Manual version

1. Get Prolog
    - On OS X, with Homebrew: `brew install swi-prolog`
    - On Ubuntu, with apt-get: `sudo apt-get install swi-prolog-nox`
2. Get git
    - On OS X, with Homebrew: `brew install git`
    - On Ubuntu, with apt-get: `sudo apt-get install git`
3. Clone and set up marelle

```bash
# clone the repo
mkdir -p ~/.local
git clone https://bitbucket.org/larsyencken/marelle ~/.local/marelle

# set up an executable
cat >~/.local/bin/marelle <<EOF
#!/bin/bash
exec swipl -q -t main -s ~/.local/marelle/marelle.pl "$@"
EOF
chmod a+x ~/.local/bin/marelle
```

## Writing deps

Make a `marelle-deps/` folder inside your project repo. Each package has two components, a `met/2` goal which checks if the dependency is met, and an `meet/2` goal with instructions on how to actually meet it if it's missing.

For example, suppose I want to write a dep for Python that works on recent Ubuntu flavours. I might write:

```prolog
pkg(python).

met(python, linux(_)) :- exists_file('/usr/bin/python').
meet(python, linux(_)) :- install_apt('python-dev').
```

To install python on a machine, I'd now run `marelle meet python`.

To install pip, I might write:

```prolog
pkg(pip).

% pip is installed if we can run it
met(pip, _) :- which(pip).

% on all flavours of linux, try to install the python-pip package
meet(pip, linux(_)) :- install_apt('python-pip').

% on all platforms, pip depends on python
depends(pip, _, [python]).
```
Note our our use of platform specifiers and the `_` wildcard in their place. To see your current platform as described by marelle, run `marelle platform`. Examples include: `osx`, `linux(precise)` and `linux(raring)`.

## Running deps

### See available deps

This runs every `met/2` statement that's valid for your platform.

`marelle scan`

### Install something

This will run the `meet/2` clause for your package, provided a valid one exists for your current platform.

`marelle meet python`

### See your platform

To find the right platform code to use in deps you're writing, run:

`marelle platform`

It reports the code for the platform you're currently on.

## Where to put your deps

Like both Babushka and Babashka, Marelle looks for deps in `~/.marelle/deps` and in a folder called `marelle-deps` in the current directory, if either exists. This allows you to set up a personal set of deps for your environment, as well as project-specific deps.

## Examples

See my [marelle-deps](https://bitbucket.org/larsyencken/marelle-deps) repo for working examples.
