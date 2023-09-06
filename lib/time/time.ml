type 'unit t  =
  | Time of float

type ('a,'b) convert =
  | Convert of float

(* 
When should we make a phantom type and when not?
For example, time could be always converted into the right "native format".
I think CISP thinks in seconds...

Why not use a set of opaque types? Because of combinatorics, if you have 4 different flavours of time,
  you will need to create 
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

let samp2float (Time samps : sample t) :> float =
  samps 

let sec2samps (Time sample_rate : sample t) :> (second t,sample t) convert =
  Convert sample_rate

let convert (Convert rate : ('a,'b) convert ) (Time time : 'a t) :> ('b t)=
  Time (time *. rate)
  
let add (Time a : 'a t) (Time b : 'a t) :> 'a t =
  Time (a +. b)

let mul (Time a : 'a t) (Time b : 'a t) :> 'a t =
  Time (a *. b)

let mup (Time a: 'a t) (factor : float) :> 'a t =
  Time (a *. factor)

let divby (Time a: 'a t) (divisor : float) :> 'a t =
  Time (a /. divisor)

let divt (Time a : 'a t) (Time b : 'a t) :> float =
   a /. b

let sub (Time a : 'a t) (Time b : 'a t) :> 'a t =
  Time (a -. b)


let addDiff rate c1 c2 =
  add c2 (convert rate c1)

let minute (x : int) :> (second t) =
  Time ((x |> float_of_int) *. 60.0)

let float_seconds (Time seconds : second t) =
  seconds

type sample_count =
  | SampleCounter of { count : int; samplerate : float }
  | Stopped

let sample_count = 
  ref Stopped

let initialize samplerate =
  sample_count := SampleCounter { count = 0; samplerate = samplerate }

let tick () = 
  sample_count := 
  match !sample_count with 
  | SampleCounter sc -> SampleCounter { count = sc.count + 1; samplerate= sc.samplerate }
  | Stopped -> failwith "Cannot run clock without providing samplerate"

let current_sample () :> (sample t) = 
  match !sample_count with
  | (SampleCounter { count ; _ }) ->
    Time (count |> float_of_int)
  | Stopped -> failwith "detected unitialized clock!"

let current_second () :> (second t) = 
  match !sample_count with
  | SampleCounter { count; samplerate } ->
    count |> float_of_int |> fun n -> n /. samplerate |> fun s -> Time s
  | Stopped ->
    failwith "detected unitialized clock !"

let now () = 
  current_second ()


  



