open Cisp
open Midi
open Seq

let midiInputTestFun input =
  (*let durPattern =
    transpose
      (ofList
         [ seq [10000; 1000; 1000]
         ; seq [4000; 5000; 60000; 3000]
         ; seq [1000; 10000; 500; 2000; 12000] ])
  in*)
  let pitchPattern =
    transpose
      (ofList
         [ seq [60; 64; 67; 72; 76; 79; 84]
         ; ch [|53; 60; 67; 74; 81|]
         ; seq (List.rev [60; 62; 64; 67; 69; 71; 72; 74; 76]) ])
    |> concat
  in
  input
  |> map (fromMidiMsgWithDur (Samps 12000))
  |> overwritePitch
       ( loop (seq [1; 1; 4; 4; 8]) (seq [8; 5; 6]) pitchPattern
       |> concat
       |> hold (st 1) )
  |> overwriteDur (ch [|44100|])
  |> overwriteChan (st 5)
  |> overwriteVelo (st 100)
  |> serialize |> map toRaw

let () =
  let state = ref (st (toRaw MidiSilence)) in
  (* the sq state var *)
  let inputRef = ref MidiSilence in
  let () = state := ofRef inputRef |> midiInputTestFun in
  let callback input =
    (* weird callback, looks like (in -> out) but reads and writes to references *)
    let out =
      match !state () with
      | Cons (curr, tl) ->
          let () = state := tl in
          curr
      | Nil -> (0, 0, 0)
    in
    let () = inputRef := fromRaw input in
    out
  in
  JackMidi.playMidi callback (ref 44100.0)
