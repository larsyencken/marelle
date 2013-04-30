# Marelle ("hopscotch")

Test-driven system administration in SWI-Prolog, in the style of [Babushka](https://github.com/babushka/babushka).

## Current status

Deliciously pre-alpha.

![Hopscotch for Seniors](https://bytebucket.org/larsyencken/marelle/wiki/img/HopscotchForSeniors.jpg)

## Installing marelle

### 1. Get prolog

On OS X, with homebrew:

```bash
brew install swi-prolog
```

On Ubuntu:

```bash
sudo apt-get install swi-prolog
```

### 2. Clone the repo

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

Make a `marelle-deps/` folder inside your project repo. Each package has two components, a `detect/2` goal and an `install/2` goal, which can be written independently.

For example, suppose I want Python installed on OS X with Homebrew. I might write a dep:

```prolog
pkg(python).
met(python, osx) :- exists_file('/usr/local/bin/python').
meet(python, osx) :- shell('brew install python').
```

## Running deps

### See available deps

This runs every `met/2` statement that's valid for your platform.

`marelle scan`

### Install something

This will run the `meet/2` clause for your package, provided a valid one exists for your current platform.

`marelle meet python`

## Where to put your deps

Like both Babushka and Babashka, Marelle looks for deps in `~/.marelle/deps` and in a folder called `marelle-deps` in the current directory, if either exists. This allows you to set up a personal set of deps for your environment, as well as project-specific deps.

## Examples

See my [marelle-deps](https://bitbucket.org/larsyencken/marelle-deps) repo for working examples.
