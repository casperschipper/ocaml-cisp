open Cisp
open Midi
open Seq
open Reader.Ops

(* simple mod of controller 1 onto pitch *)

let sr = ref 44100.0


 
(* todo abstract this to functiion that only takes channel and controller *)
let getController =
  Reader.ask () >>= (fun env -> MidiState.getControllerValue (MidiCh 0) 1 env |> Reader.return)

let triggered =
  Reader.ask () >>= (fun env -> MidiState.triggerFromCurrent 3000 env |> Reader.return )

let pitchControl =
  getController >>= (fun ctrl1 ->
    triggered >>= (fun evt -> evt |>
                                mapOverPitch (fun p -> p + ctrl1) |>
                                 mapOverDuration (fun d -> (Float.of_int d) *. (Float.of_int ctrl1 /. 128.0) |> Int.of_float)
                                 |> Reader.return))
  
(* TODO recursive clock mapping *)            
(* this maps midi input msg to an output msg (raw midi) *)
let midiInputTestFun input =
  input
  |> MidiState.makeSeq
  |> map (Reader.run pitchControl)
  |> serialize
  |> map toRaw
                         
let () = Midi.playMidi midiInputTestFun sr 
           
