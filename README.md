# ocaml-cisp

An attempt to do something similar to my language CISP in a more type safe & functional way using ocaml. 
The central datatype used is ocaml's Seq.t, which is a "lazy" sequence which can represent audio streams, midi or more higher level concepts.

I took the JACK bindings and some other basic elements from doebereiners "processes".
The main functions can be found in cisp.ml, examples cisp1 cisp2 etc.. are the current experiments.

Has been used for the interactive piece "decoherence" (see <https://klangraum-dumpf.com/>) and for various jams together with Olaf Kerkhaert, mainly using the midi functionality.

Core modules:

- cisp (the seq function stuff, with algorithmic composition building blocks, schedulers, sequencers, random generators, oscillators, filters etc.. mostly targeted at a compositionally motivated approach to sound synthesis)
- midi (parse and generate midi)
- jack (to play audio)
- jackMidi (to play midi)
- parser, a parser combinator
- quip, a lisp compiler, for on-the-fly coding intended to be used through a web interface ([cisp-lab](https://github.com/casperschipper/cisplab), writtin in Elm).


# limitations

This all was written while learning ocaml, there is some duplication of functionality throughout, I hope to simplify in the near future once I have decided which patterns I like best.

The jack bindings work, but only jackMidi works with ocaml threads at the moment. There is a bit of statefullness there, that I no longer believe is necessary, but it works ok.

Ocaml has a garbage collector, and although it is not as aggresive as those in Java or javascript, this still will cause bottlenecks when writing heavy audio. This library is aimed at making it easy to do non-standard synthesis experiments, if you want optimization, look elsewhere (Supercollider, Rust?).

# Running

You will need:

* [opam](https://ocaml.org/docs/up-and-running)

Opam allows you to install a ocaml compiler and packeges.
* ocaml 4.14 + flambda

You will also need the c library jacklib (brew has a package)

# packages used

`opam install *package-name*`

* dune
* utop (optional, but nice)
* (ctypes)
* ctypes-foreign
* bigarray
* [lwt](https://opam.ocaml.org/packages/lwt/) 
* lwt.unix
* [lo](https://opam.ocaml.org/packages/lo/) for OSC support

# Build and run

You need to have a running jack server (there is also a runJack.sh script I use for this).

Make any of the examples by doing dune exec ./cisp1.exe 
*note the local `./` path prepending the program name*

for example:
`dune exec ./cisp1.exe`



