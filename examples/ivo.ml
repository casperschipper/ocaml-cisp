module S = Lo.Server
module R = Result

type preset = Preset of {startT: float; endT: float}

type data = Lo.Message.data

type state = Presets of preset Array.t

type 'a oscParser =
  | OscParser of (data Seq.t -> ('a * data Seq.t, string) Result.t)

let run (OscParser p) osc = p osc

(* which is a monad *)
let return a = OscParser (fun s -> Result.ok (a, s))

(*   fmap f (Parser cs) = Parser (\s -> [(f a, b) | (a, b) <- cs s]) *)
let fmap f (OscParser p) =
  let mapFst f (a, s) = (f a, s) in
  OscParser (fun s -> p s |> Result.map (mapFst f))

(*
    
let join_result res_sq =
  let module R = Result in
  let rec aux in_sq acc =
    match acc with
    | Error err -> Error err
    | Ok accu ->
      begin
        match in_sq () with
        | Seq.Nil -> Ok accu
        | Seq.Cons(R.Ok sq,tail) ->
          let newAccu = R.Ok (Seq.append sq accu)in
          aux tail newAccu
        | Seq.Cons(R.Error err,_) ->
          R.Error err
      end
  in
  aux res_sq (R.Ok Seq.empty)
*)

let bind p f =
  let step result =
    match result with
    | R.Ok (a, rest) -> run (f a) rest
    | R.Error error -> R.Error error
  in
  OscParser
    (fun s ->
      let r1 = run p s in
      step r1 )

let ( >>= ) = bind

(* and applicative *)
let ( >> ) m k = m >>= fun _ -> k

let ( <*> ) fP xP = fP >>= fun f -> xP >>= fun x -> return (f x)

let failure err = OscParser (fun _ -> Result.Error err)

let item =
  let open Seq in
  OscParser
    (fun oscSq ->
      match oscSq () with
      | Nil -> Result.error "reached unexpected end"
      | Cons (x, xs) -> Result.Ok (x, xs) )

let satisfy predicate error =
  item >>= fun c -> if predicate c then return c else failure error

let option p q =
  OscParser
    (fun s ->
      let res = run p s in
      match res with R.Error _ -> run q s | R.Ok res -> R.Ok res )

let isString str data = match data with `String s -> s = str | _ -> false

let float =
  item
  >>= fun d ->
  match d with `Float f -> return f | _ -> failure "expected a float"

let int =
  item
  >>= fun d ->
  match d with `Int32 i -> return i | _ -> failure "expected an int"

let floatParameter parName =
  let err = "Expected parameter name: " ^ parName in
  satisfy (isString parName) err >> float

let preset =
  let mkPreset start_t end_t = Preset {startT= start_t; endT= end_t} in
  return mkPreset <*> floatParameter "start" <*> floatParameter "end"
