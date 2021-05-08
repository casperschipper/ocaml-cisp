open Cisp
open Midi
    
let intervArr = [|50;53;55;57;60;62;64;67;69|]
let copy = Array.copy intervArr

let maxidx = Array.length intervArr 

let writer =
  let idx = rv (st 0) (st maxidx) in
  let f old original =
    let nw = old + (pickOne [|-2;2|]) in
    pickOne [|nw;original|] 
  in
  idx |> Seq.map (fun i -> intervArr.(i) <- f intervArr.(i) copy.(i)) |> syncEffectClock (interval (st 1)) 
  

let midiReader =
  let ( let* ) x f = Reader.bind f x in
  let* trigger = MidiState.boolFromNote in
  Reader.return trigger

let simpleWalk = 
  let ( +~- ) a b = Seq.map ( ( + ) a ) b in 
  let loopy = seq [0;3;6;9] |> Seq.map (fun x -> x +~- (seq [0;1;0;2]))  in
  let holdn = seq (shuffle [11;7;5]) in
  let offset = walki 2 (ch [|(-1);1|]) |> Seq.map (wrap 0 maxidx) |> hold holdn in
  let zipper l off = l +~ (st off |> take 3) in
  zipWith zipper loopy offset |> concat 


let oneWalk = 
  let idx = simpleWalk in
  index intervArr idx




(*type walkmind direction is state *)
(* a walk that updates boundaries only when hit *)

let map = Seq.map
 
let notes channel =
  st makeNoteOfInts
  <*> (oneWalk)
  <*> (st 80)
  <*> (st (seci (0.1)))
  <*> (st channel)

let ofTrigger channel trig =
  weavePattern trig (map Option.some (notes channel)) (st None)

(* this clock only produces integer on the trigger 
t f f f t f f f t f f f 
[Some 0;None;None;None;None;Some 1;None;None;None;None;Some 2]
 *)
let countClock clickTrack =
   weavePattern clickTrack (Seq.map Option.some count) (st None)
    
let withDefault default opt =
  match opt with
  | Some x -> x
  | None -> default

(* this takes an optSq and a test (f), returns false unless test is true 
   interesting in combination with testClock
*)
let mapOverOpt f optSq =
  let g x = x |> Option.map f |> withDefault false in
  optSq |> Seq.map g

(* checks if equal *)
let modPulse n optSq =
  let f x = x mod n = 0 in
  mapOverOpt f optSq

type boolSieve =
  BoolSieve of bool Seq.t

let mkBoolSieve sq =
  BoolSieve sq
    

type sieveType =
  | Union
  | Difference
  | Intersection
  | Exclusive

let rec gcd a b =
  if b = 0 then
    a
  else
    gcd b (a mod b)

let divrem a b =
  let x = a / b in
  (x,a-(b*x))

let rec repeatValue n a =
  if n = 0 then
    []
  else
    List.cons a (repeatValue (n - 1) a)

let list_return a =
  [a]

let rec merge (result,lsta,lstb) =
  match lsta with
  | [] -> (result,[],lstb)
  | x :: xs -> 
    match lstb with
    | [] -> (result,[],lsta)
    |  y::ys -> (merge (List.append x y :: result,xs,ys))

type beat =
  | X
  | R  

let euclidRhythm a b =
  if a > b then
    []
  else
    let (xs,ys) = (repeatValue a X |> List.map list_return,repeatValue (b-a) R |> List.map list_return) in
    let rec compute merged remain =
      match merge ([],merged,remain) with
      | (result,[],[]) -> result
      | (result,[],rest) -> compute result rest
      | (result,_,_) -> result 
    in
    compute xs ys |> List.flatten

let printEuclid lst =
  List.fold_right (fun x acc -> match x with
                               | X -> "x" ^ acc
                               | R -> "." ^ acc) lst ""

let toBool x =
  x = X

let euclidTrigger a b =
  euclidRhythm a b |> List.map toBool |> List.to_seq |> cycle

(*
[]    [[1];[1];[1];[1];[1];[1];[1]] [[0];[0];[0];[0];[0];[0];[0];[0];[0]]

[[10]] [[1];...] [[0];[0];]


[1;1;1;1;1;1;1] [0;0;0;0;0;0;0;0;0;0;0;0]

[10;10;10;10;10] [0;0;0;0]

[100;100;100;100] [10]

[100;100;100;100] []

      [10;10;10;10;10;10;10] [0;0]

      [10;10;10;0] [10;10;10;0] []


      [1;1;1;1;1] [0;0;0] 5 * 1 + 3

      [10;10;10] [1;1] 3 * 2 + 2

      [101;101] [10] 2 * 3 + 1

      [101;101;10]

    [1;1;1;1;1;1] [0;0]

      [1;1;0] [1;1;0] [1;1]
   [1;1;0;1] [1;1;0;1] []

    [1;1;1;1;1] [0;0;0]

      [10;10;10] [1;1]

      [101;101] [10]

      [101;101;10] []


[1;1;1;1;1;1;1] [

   
5 / 8

5 + 3
3 + 2
2 + 1

*)

let sieve sieveCombinator (BoolSieve a) (BoolSieve b) =
  match sieveCombinator with
  | Union -> zipWith (||) a b
  | Difference -> zipWith (<>) a b
  | Intersection -> zipWith (&&) a b
  | Exclusive -> zipWith (fun x y -> match (x,y) with (true,false) -> true | (_,_) -> false) a b

let makeBundles (trigSq : bool Seq.t ) =
  let cnt = trigSq |> countClock in
  let ns = [4;5;6;7;11;17] in
  let aSq n =
    cnt
    |> modPulse n
    |> ofTrigger 1
  in
  let addOptToBundle opt bundle =
    match opt with
    | Some evt -> addToBundle bundle evt
    | None -> bundle
  in
  ns |> List.map aSq |> list_fold_heads_with silenceBundle addOptToBundle
  

let midiFun input =
  input
  |> MidiState.makeSeq
  |> map (Reader.run midiReader)
  |> makeBundles
  |> effectSync writer
  |> serializeBundles
  |> map toRaw


let () = Midi.playMidi midiFun Process.sample_rate 
