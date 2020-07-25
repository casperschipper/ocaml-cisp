open Cisp
open Midi
open Seq

let threeLists = [[11; 12; 13]; [42; 43; 44]; [100; 99]]

let seqs = List.map (fun lst -> ofList lst |> cycle) threeLists

let l_seqs = ofList seqs (* make this list also a lazy stream *)

let addCount () = Cons (count, l_seqs)

(* this adds in another infinite list, wich counts natural numbers *)

let trans = transpose addCount

let cat = trans |> concat |> take 30 |> toList

let testSah = lift rvf (-0.5) 0.5 |> hold (trunc (lift rvf 1.0 1000.0))

let a = lift rvf (-1.0) 1.0

let b = lift rvf 1.0 10.0

let myLine = line a b

let abcq =
  let a = List.to_seq [1; 2; 3] in
  let b = List.to_seq [20; 21; 22] in
  let c = List.to_seq [100; 102] in
  let d = List.to_seq [] in
  let ef = countFrom 999 |> take 10 in
  List.to_seq [a; b; c; d; ef]

let myLoopyLines =
  myLine ()
  |> loop (ch [|2; 4; 17; 23; 1111|]) (ch [|2; 4; 16; 32; 64; 128|])
  |> concat

(* todo *)
let holder =
  countFrom 1 |> map collatz |> concat
  |> map (fun x -> 44.1 /. Float.of_int x)
  |> map (clip 1. !samplerate)
  |> trunc

let collatzSynth = seq [-1.0; 0.5; 0.; 0.5; 1.0] |> hold holder

let fishFreqs =
  countFrom 1 |> map collatz |> concat
  |> map (fun x -> (x mod 64) + 64)
  |> floatify |> map mtof
  |> map (fun x -> !samplerate /. x)
  |> trunc

let table = genSine 1024

let fish = waveOscL table 100.

let test testInputSq callback nframes =
  testInputSq |> callback |> take nframes
  |> iter (fun msg -> print_midi_msg (fromRaw msg))

(* let () = test (testSequence ()) midiInputTestFun 1024 *)

let () = run ()
