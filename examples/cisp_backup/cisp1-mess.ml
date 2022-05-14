open Cisp
open Midi
open Seq


(* simple mod of controller 1 onto pitch *)

let sr = ref 44100.0

       (*
let pitchControl =
  MidiState.getControlR (MidiCh 0) (MidiCtrl 0) >>= fun (MidiVal ctrl1) ->
  MidiState.getControlR (MidiCh 0) (MidiCtrl 1) >>= fun (MidiVal ctrl2) ->
  MidiState.triggerR 3000 >>= fun evt ->
  (ctrl1,ctrl2, evt) |> Reader.return
        *)

(* could it just be a map over the Reader ? *)



  (* 
let pitchControl2 =
  let (let* ) x f = Reader.bind f x in
  let* (MidiVal ctrl1) = MidiState.getControlR (MidiCh 0) (MidiCtrl 2) in
  let* (MidiVal ctrl2) = MidiState.getControlR (MidiCh 0) (MidiCtrl 3) in
  let* evt = MidiState.triggerR 9000 in
  Reader.return (ctrl1,ctrl2, evt) *)

type data =
  { c1 : int; p : pitch }

let currentState = ref ({ c1 = 0 ; p = pitchOfInt 0 })
       
let pitchControl3 =
  let (let* ) x f = Reader.bind f x in
  let* (MidiVal ctrl1) = MidiState.getControlR (MidiCh 0) (MidiCtrl 1) in
  let* pitch = MidiState.getPitch in
  let* trigger = MidiState.boolFromNote in
  let () = match pitch with
    | Some p -> currentState := { c1 = ctrl1 ; p = p }
    | None -> currentState := { !currentState with c1 = ctrl1 }
  in
  Reader.return ( trigger )
           

let rec valuesOnly sq () =
    match sq () with
    | Cons((Some data) ,tail) -> Cons(data,valuesOnly tail)
    (* this line makes a lot of trouble *)
    | Cons(None, tail) -> valuesOnly tail ()
    | Nil -> Nil
  
let rec mapOverOpt f opts sq () =
  match opts () with
  | Nil -> Nil
  | Cons( Some value, tail) ->
     begin
     match sq () with
     | Nil -> Nil
     | Cons( sqHead, sqTail) -> Cons( f value sqHead, mapOverOpt f tail sqTail )
     end
  | Cons( None, tail) ->
     Cons ( None, mapOverOpt f tail sq)

let ofTrigger =
  map (fun trig -> match trig with
                   | true -> mkNoteClip 1 60 100 500
                   | false -> SilenceEvent ) 
    (* 
let ofState opts =     
  let sq = valuesOnly opts in

  let pst = map (fun data -> data.p |> fun (Pitch p) -> p) sq in
  (*
  let myWalk = walki 0 c1 in
  let arr = [|0;2;4;5;7;12;4|] in 
  let indexed = index arr myWalk in*)

  let defaultNote = (mkNoteClip 1 60 100 500 |> st) in
  zipWith (fun p evt -> mapOverPitch (fun _ -> p) evt) pst defaultNote  
let rec mapOpts f opts () =
  match opts () with
  | Nil -> Nil
  | Cons(x, ls) ->
     let rs = mapOpts f ls in
     match f x with
     | None -> rs ()
     | Some v -> Cons(v, rs)

type eventState = { currentEvent : midiEvent option
                  ; eventStream : midiEvent Seq.t }


(* 1 0 0 0 1 0 0 0 0 1 0 0 0 0 1 0 0 0  *)

   
            (*     
let mapOverOpt2 f opts =
  recursive
    opts
    { currentEvent = None
    ; eventStream = f opts }
    (fun control state ->
      match control with
      | Some _ ->
         begin    
         match state.eventStream () with
         | Nil -> { state with currentEvent = Some c4 }
         | Cons(evt, tail) -> { currentEvent = Some evt; eventStream =  tail }
         end
      | None -> { state with currentEvent = None })
    (fun state -> state.currentEvent) *)
         
                            
                           
           
(* hopefully obsolete trash ?   
                         
let mapOverOpt f opts =
  let values = mapOpts id opts in
  let mapped = f values in
  let rec reconstruct opts values () =
    match (opts (), values ()) with
    | (Cons(Some _, tail), Cons(v, rest)) ->
       Cons(Some v, reconstruct tail rest)
    | (Cons(None, tail), _) ->
       Cons (None, reconstruct tail values)
    | (_,_) ->
       Nil
  in
  reconstruct opts mapped
  *)

  (*
let ofTuple tup =
  let (c1,c2,evt) = unzip3 tup in (* a seq of (x,y) make it (seq x, seq y) *)

  let ctl1 = ref 0 in
  let ctl2 = ref 0 in

  let mywalk = walki 0 (ofRef ctl1) in
  let arr = [|0;2;4;5;7;12;4|] in
  let indexed = index arr mywalk in

  
  let mywalk2 = walki 0 (ofRef ctl2) in
  let arr2 = [|(-12);0;7;12;24;7|] in
  let indexed2 = index arr2 mywalk2 in
  
  evt
  |> effect (wrRef ctl1 c1)
  |> effect (wrRef ctl2 c2)
  |> overwritePitch (indexed +~ indexed2 +~ (st 60))
  |> overwriteVelo (seq [0; 100])
  |> overwriteChan (st 2)*)

(* [ option a ] -> ( [ a ] -> [ b ] ) -> [ option b ] *)

(* some a, some b, none, some c, come d *)


  
(* 1, 2, none, none, 1, 2, none, none *)
     *)

  
(* this maps midi input msg to an output msg (raw midi) *)
let midiInputTestFun input =
  input
  |> MidiState.makeSeq (* take msg, make it a state *)
  |> map (Reader.run pitchControl3) (* run a bunch of readers to extract properties *)
  |> ofTrigger
  |> serialize |> map toRaw (* turn back into raw midi *)
             
  
let () = Midi.playMidi midiInputTestFun sr 
           
