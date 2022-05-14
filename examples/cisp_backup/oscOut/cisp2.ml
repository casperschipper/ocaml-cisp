open Cisp
open Midi
open Seq

let sr = ref 44100.0

let channel = 1 

(* this maps midi input msg to an output msg (raw midi) *)
let midiInputTestFun input =
  (* this translates input into boolean trigger *)
  (* a metre boolean mask that triggers notes or rests in the pattern *)
  (* dummy event *)
  (* apply metre to default event, use Silence as filler *)
  let pitchPattern =
    transpose
      (Cisp.ofList
         [ seq [60; 72]
         ; rv (st 60) (st 63)
         ; line (seq [60;72] |> floatify) (seq [60;64;72;58] |> floatify) |> trunc
         ]) |> concat
  in
  input 
  |> map (fromMidiMsgWithDur (Samps 4200))
  |> overwritePitch pitchPattern
  |> withDur (ch [|4250|])
  |> withChan (st 2)
  |> withVelo (st 100)
  |> serialize |> map toRaw


let () = Midi.playMidi midiInputTestFun sr
