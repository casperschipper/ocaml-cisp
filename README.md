# ocaml-cisp

An attempt to do something similar to my language CISP in a more type safe & functional way using ocaml. 
The central datatype used is ocaml's Seq.t, which is a "lazy" sequence which can represent audio streams, midi or more higher level concepts.

I took the JACK bindings and some other basic elements from doebereiners "processes".
The main functions can be found in cisp.ml, examples cisp1 cisp2 etc.. are the current experiments.

Has been used for the interactive piece "decoherence" (see <https://klangraum-dumpf.com/>) and for various jams together with Olaf Kerkhaert, mainly using the midi functionality.


