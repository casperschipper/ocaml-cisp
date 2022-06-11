type 'unit t  =
  | Time of float

type ('a,'b) convert =
  | Convert of float

(* 
When should we make a phantom type and when not?
For example, time could be always converted into the right "native format".
I think CISP thinks in seconds...
*)

type millisecond

type second

type minute

type day

type sample

type year

let min2sec :> (minute t,second t) convert =
  Convert 60.0

let sec2ms :> (millisecond t,second t) convert = 
  Convert 1000.0
  
let ms2sec :> (second t,millisecond t) convert = 
  Convert 0.001



let convert (Convert rate : ('a,'b) convert ) (Time time : 'a t) :> ('b t)=
  Time (time *. rate)
  
let add (Time a : 'a t) (Time b : 'a t) :> 'a t =
  Time (a +. b)

let mul (Time a : 'a t) (Time b : 'a t) :> 'a t =
  Time (a *. b)

let div (Time a : 'a t) (Time b : 'a t) :> 'a t =
  Time (a /. b)

let sub (Time a : 'a t) (Time b : 'a t) :> 'a t =
  Time (a -. b)


let addDiff rate c1 c2 =
  add c2 (convert rate c1)

let minute (x : int) :> (second t) =
  Time ((x |> float_of_int) *. 60.0)

let float_seconds (Time seconds : second t) =
  seconds



  



