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
detect(python, osx) :- exists_file('/usr/local/bin/python').
install(python, osx) :- shell('brew install python').
```

## Running deps

### See what's installed

This runs every `detect/2` statement that's valid for your platform.

`marelle installed`

### Install something

This will run the `install/2` clause for your package, provided a valid one exists for your current platform.

`marelle install python`
