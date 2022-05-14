open Cisp
open Midi
open Seq

let sr = ref 44100.0

(* this maps midi input msg to an output msg (raw midi) *)
let midiInputTestFun input =
  (* this translates input into boolean trigger *)
  (* a metre boolean mask that triggers notes or rests in the pattern *)
  (* dummy event *)
  (* apply metre to default event, use Silence as filler *)
  let pitchPattern =
   Cisp.ofList
         [ ch [|60;72|]
         ; ch [|60;67|]
         ; ch [|74;76;78|]
         ; ch [|60;60;80;70|]
         ] |> 
     transpose
       |> concat |> loop (st 3) (st 4) |> concat
  in
  input 
  |> map (fromMidiMsgWithDur (Samps 4200))
  |> overwritePitch pitchPattern
  |> withDur (ch [|4250|])
  |> withChan (st 3)
  |> withVelo (st 100)
  |> serialize |> map toRaw


let () = Midi.playMidi midiInputTestFun sr
