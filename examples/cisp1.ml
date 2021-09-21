open Cisp
open Midi

let map = Seq.map

let ofTrigger lst trigger =
  let notes =
    seq lst
    |> floatify
    |> ( ( +.- ) 48.0 )
    |> map (fun p -> p |> Int.of_float |> mkPitchClip)
    |> map (fun p -> c3 |> withPitch p |> withDur (seconds 0.1))
  in
  Cisp.weave trigger (notes) (SilenceEvent |> st) 
  

let thread lst z d trig =
  let zeros = st 0 |> take z in
  let ds = st 1 |> take d in
  let n = [zeros;ds] |> List.to_seq |> transpose |> cycle |> concat in
  trig
  |> pulseDivider n
  |> ofTrigger lst

let toTrigger =
  map Midi.isNoteOn

let mergeMidiStreams sqa sqb =
  (* this merges streams of midi events, so it becomes one stream of events. When two events "collide", it pushes one onto the stack.
     events on the stack have priority on newer events
   *)
  let module Q = Fqueue in
  let handleEvents (a,b) (overflow,_) =
    let optFlow =
      Q.peekdeq overflow
    in
    match (a,b,optFlow) with
    | (SilenceEvent, SilenceEvent, None) -> (Q.empty,SilenceEvent)
    | (SilenceEvent, SilenceEvent, Some (evt,rest)) -> (rest, evt) 
    | (SilenceEvent, evtb, Some (evt,rest)) -> (Q.enqueue evtb rest,evt)
    | (evta, SilenceEvent, Some (evt,rest)) -> (Q.enqueue evta rest,evt)
    | (evta, SilenceEvent, None) -> (Q.empty,evta)
    | (SilenceEvent, evtb, None) -> (Q.empty,evtb)
    | (evta, evtb, None) -> (Q.enqueue evtb Q.empty, evta)
    | (evta, evtb, Some(evt, rest)) ->
       (rest |> Q.enqueue evta |> Q.enqueue evtb, evt) 
       
  in
  let zipped = Cisp.zip sqa sqb in
  recursive zipped (Q.empty, SilenceEvent) handleEvents (fun (_,evt) -> evt)

let midiFun input =
  let sqa = input |> toTrigger |> thread [0;5;10;15;0;7;14;21] 3 1 in
  let sqb = input |> toTrigger |> thread [0;5;10;15;0;7;14;21] 3 2 in
  let sqc = input |> toTrigger |> thread [0;12;(-12);0;7;(-7);(-14)] 3 3 in
  mergeMidiStreams sqa sqb |> mergeMidiStreams sqc
  |> serialize 
  |> map toRaw

  
let () =
  let f () =
    Midi.playMidi midiFun Process.sample_rate 
    ; while true
      do
        Unix.sleep 60
      done
  in
  let _ = Thread.create f () in
  let _ = Sys.command "jack_disconnect system_midi:capture_2 ocaml_midi:ocaml_midi_in" in
  let _ = Sys.command "jack_disconnect ocaml_midi:ocaml_midi_out system_midi:playback_1" in
  let _ = Sys.command "jack_connect ocaml_midi:ocaml_midi_out system_midi:playback_1" in
  let _ = Sys.command "jack_connect system_midi:capture_2 ocaml_midi:ocaml_midi_in" in
  while true
  do
    Unix.sleep 60
  done

   

    
    


  
  
            
 
