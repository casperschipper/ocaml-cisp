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

This all was written while learning ocaml, there is some duplication of functionality throughout, I hope to simplify in the near future once I have decided which patterns I like best.
The jack bindings work, but only jackMidi works with ocaml threads at the moment.



