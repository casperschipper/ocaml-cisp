# Processes
OCaml libraries for functional sound synthesis

## Installation

The libraries have been tested and developed using the
```4.09.0+flambda``` compiler variant available on ```opam```. Further
requirements are ```ctypes.foreign``` and ```dune``` (installable via
```opam```) and the jack audio library (e.g. using the package
```libjack-dev```).

Once these requirements are installed, the tests can be compiled by
running (from inside the ```example``` directory):

```
$ dune build simplesine.ml
```

or, after starting jack, by compiling and running test programs at once:

```
$ dune exec ./simplesine.exe
```

## Documentation

Further documentation can be found in the ```doc``` directory.

## Examples 

Further examples can be found in the ```examples``` directory. See the
```dune``` file in the ```examples``` directory for information on how
to set up and compile your own projects using these libraries.
