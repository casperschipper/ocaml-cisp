type data = Lo.Message.data

module R = Result

(* A table as in *)
type floats = float Array.t

let set (idx:int) (flt:float) (arr:floats) =
  Array.set arr idx flt 

let get idx (arr:floats) =
  arr.(idx)


type 'a t = OscParser of (data Seq.t -> ('a * data Seq.t, string) Result.t)

let ofData = function
  | `Int32 n -> string_of_int n
  | `Float f | `Double f -> Printf.sprintf "%f" f
  | `String s -> s
  | `True -> "True"
  | `False -> "False"
  | _ -> "???"

let run (OscParser p) osc = p osc

(* which is a monad *)
let return a = OscParser (fun s -> Result.ok (a, s))

(*   fmap f (Parser cs) = Parser (\s -> [(f a, b) | (a, b) <- cs s]) *)
let fmap f (OscParser p) =
  let mapFst f (a, s) = (f a, s) in
  OscParser (fun s -> p s |> Result.map (mapFst f))

let bind p f =
  let step result =
    match result with
    | R.Ok (a, rest) -> run (f a) rest
    | R.Error error -> R.Error error
  in
  OscParser
    (fun s ->
      let r1 = run p s in
      step r1)

let ( >>= ) = bind

(* and applicative *)
let ( >> ) m k = m >>= fun _ -> k

let ( <*> ) fP xP =
  fP >>= fun f ->
  xP >>= fun x -> return (f x)

let failure err = OscParser (fun _ -> Result.Error err)

let item =
  let open Seq in
  OscParser
    (fun oscSq ->
      match oscSq () with
      | Nil -> Result.error "reached unexpected end"
      | Cons (x, xs) -> Result.Ok (x, xs))

let satisfy predicate error =
  item >>= fun c -> if predicate c then return c else failure error

let option p q =
  OscParser
    (fun s ->
      let res = run p s in
      match res with R.Error _ -> run q s | R.Ok res -> R.Ok res)

let isString str data = match data with `String s -> s = str | _ -> false

let float =
  item >>= fun d ->
  match d with
  | `Float f | `Double f -> return f
  | _ -> failure "expected a float"

let int =
  item >>= fun d ->
  match d with `Int32 i -> return i | _ -> failure "expected an int"

let string =
  item >>= fun s ->
  match s with `String s -> return s | _ -> failure "expected a string"

let bool =
  item >>= fun s ->
  match s with
  | `True -> return true
  | `False -> return false
  | _ -> failure "expected a bool"

let flt_par parName =
  let err = "Expected parameter name: " ^ parName in
  satisfy (isString parName) err >> float

let int_par parName =
  let err = "Expected int parameter with name: " ^ parName in
  satisfy (isString parName) err >> int

let str_par parName =
  let err = "Expected string parameter with name: " ^ parName in
  satisfy (isString parName) err >> string

let bool_par parName =
  let err = "expected bool parameter with name: " ^ parName in
  satisfy (isString parName) err >> bool
