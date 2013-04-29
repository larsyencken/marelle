# Marelle ("hopscotch")

Test-driven system administration in SWI-Prolog, in the style of [Babushka](https://github.com/babushka/babushka).

## Current status

Deliciously pre-alpha.

## Installing marelle

### On OS X, with Homebrew

```bash
# install prolog
brew install swi-prolog

# clone the repo
mkdir -p ~/.local
git clone https://bitbucket.org/larsyencken/marelle ~/.local/marelle
ln -s ~/.local/marelle/marelle ~/.local/bin/marelle
```

### On Ubuntu

```bash
# install prolog
sudo apt-get install swi-prolog

# clone the repo
mkdir -p ~/.local
git clone https://bitbucket.org/larsyencken/marelle ~/.local/marelle
ln -s ~/.local/marelle/marelle ~/.local/bin/marelle
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
