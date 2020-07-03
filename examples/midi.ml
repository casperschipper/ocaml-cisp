(* Midi Out *)

let foo = List.to_seq [1.0; 2.0; 3.0]

let _ = JackMidi.playMidi foo (ref 0.0)
