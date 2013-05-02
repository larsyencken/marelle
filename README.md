# Marelle ("hopscotch")

Test-driven system administration in SWI-Prolog, in the style of [Babushka](https://github.com/babushka/babushka).

## Current status

Deliciously pre-alpha.

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

### See your platform

To find the right platform code to use in deps you're writing, run:

`marelle platform`

It reports the code for the platform you're currently on.

## Where to put your deps

Like both Babushka and Babashka, Marelle looks for deps in `~/.marelle/deps` and in a folder called `marelle-deps` in the current directory, if either exists. This allows you to set up a personal set of deps for your environment, as well as project-specific deps.

## Examples

See my [marelle-deps](https://bitbucket.org/larsyencken/marelle-deps) repo for working examples.
