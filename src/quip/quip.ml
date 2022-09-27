open Parser

(*
   mainly a port of
   tomotakatakahashi's https://github.com/tomotakatakahashi/lispy-elm
   which in turn is based on
   http://www.norvig.com/lispy.html

   open questions:
   How to deal with (1 2 3 4) ((1 2) (3 4) (5 6))

  Problem (seq 2 (cycle 11 12) 3) interprets 2 and 3 as infinite streams, but for seq that doesn't make sense

  Some observations about streams.contents
  The old Seq function actually understood integers as infinite streams of the same number, therefore
  Seq would transpose these: (seq 1 2 3) is thus 1 2 3 1 2 3 1 2 3  etc...

  However, when one has finite streams, seq 1 2 3 would result in 1 2 3 . end
  

*)

let assert_equal label a b =
  if a = b then print_string "all ok"
  else print_string ("failed: " ^ label ^ " <<<should be>>> ")

type problem = Problem of string

(* Stream of Stream is just stream Seq.t *)
type stream =
  | InfStream of float Infseq.t
  | FinStream of float Seq.t
  | FinStreams of float Seq.t Infseq.t
  | FinFinStreams of float Seq.t Seq.t

type list_type = FloatList | FinList | InfList | Impossible of string

type quiplist =
  | FloatLst of float List.t
  | FinStreamLst of float Seq.t List.t
  | InfStreamLst of float Infseq.t List.t
(* | LstLst of quiplist List.t *)

(*


   (seq 1 2 3 (seq 10 11 12)) -> 1 2 3 10 1 2 3 11 1 2 3 12
   (seq 1 2 3 (seq 10 11 12)) -> 1 2 3 10 11 12 1 2 3 10 11 12

   (cat (seq 10 11 12) (seq 1 2) (seq 11 12 13))

   observation:
   only finite streams can be catted
   catting flattens finite streams into one finite stream

   transcat makes a weave out of infinite streams (maybe makes less sense as finite streams)

   (trans (rv 1 100) (ch 1 2 3 4) ())
*)

(*

   type stream =
     | InfStream of float Infseq.t
     | FinStream of float Seq.t
     | FinStreams of stream InfSeq.t
     | FinFinStreams of stream Seq.t
*)

let stream_to_string = function
  | InfStream _ -> "inf stream"
  | FinStream sq ->
      "finstream: "
      ^ (Seq.map string_of_float sq |> List.of_seq |> String.concat "\t")
  | FinStreams sqqs ->
      let snip = sqqs |> Infseq.take 5 in
      let str =
        snip
        |> Seq.map (fun sq ->
               sq |> Seq.map string_of_float |> List.of_seq
               |> String.concat "\t")
        |> List.of_seq |> String.concat "\n"
      in
      "an infinite number of finite streams\n" ^ str
  | FinFinStreams sqqs ->
      sqqs
      |> Seq.map (fun sq -> "seq of length: " ^ string_of_int (Seq.length sq))
      |> List.of_seq |> String.concat "\t"

let problemize p = Problem p

type expression =
  | Constant of Parser.number
  | Symbol of string
  | List of expression list
  | Lambda of expression list * expression
  | Function of (expression list -> (expression, problem) result)
  | TrueExpression
  | Stream of stream

let stream stream = Stream stream
let fin_stream stream = Stream (FinStream stream)
let inf_stream stream = Stream (InfStream stream)

let rec expression_to_string exp =
  match exp with
  | Constant n -> Parser.number_to_string n
  | Symbol str -> str
  | List lst ->
      "(" ^ (lst |> List.map expression_to_string |> String.concat " ") ^ ")"
  | Lambda (lst, body) ->
      "lambda | with parameters: \n "
      ^ expression_to_string (List lst)
      ^ "\n" ^ "| and expression: \n" ^ " " ^ expression_to_string body ^ "\n"
  | Function _ -> "function "
  | TrueExpression -> "#true"
  | Stream stream -> stream_to_string stream

let expressions_to_string lst =
  Ok (Symbol (lst |> List.map expression_to_string |> String.concat "\n"))

let monoform (lst : expression list) =
  (*
        This function turns a diverse list into a single type, type normalisation
       For example, if a list is a mix of floats and streams, everything is made into a stream.
       If something starts with a float, but along the way there is a single stream, it will be stream list, since that is the more fundamental type in Cisp.
       We assume that the most longest type is the intented type

      float float float

      float fin float
      >>
      fin fin fin (should fin be extended to fit the shortest ???)


      float inf fin
  *)
  let rec find_type xs state =
    (* this goes over the list *)
    match xs with
    | [] -> state
    | x :: rest -> (
        match x with
        | Constant _ -> (
            match state with
            | FloatList -> find_type rest FloatList
            | FinList -> find_type rest FinList
            | InfList -> find_type rest InfList
            | Impossible str -> Impossible str)
        | Stream (InfStream _) -> (
            match state with
            | FloatList -> find_type rest InfList
            | FinList -> find_type rest FinList
            | InfList -> find_type rest InfList
            | Impossible str -> Impossible str)
        | Stream (FinStream _) -> (
            match state with
            | FloatList -> find_type rest FinList
            | FinList -> find_type rest FinList
            | InfList -> find_type rest FinList
            | Impossible str -> Impossible str)
        | Stream (FinFinStreams _) -> Impossible "finfin streams not supported"
        | Stream (FinStreams _) -> Impossible "finstreams not supported"
        | _ -> Impossible "these are not streams")
  in
  let make_float item =
    match item with Constant num -> Parser.number_to_float num | _ -> 0.0
  in
  let make_finite_sq item =
    match item with
    | Constant num -> Parser.number_to_float num |> Cisp.st
    | Stream (FinStream sq) -> sq
    | Stream (InfStream sq) -> sq |> Infseq.to_seq
    | _ -> Cisp.st 0.0
  in
  let make_infinite_sq item =
    match item with
    | Constant num -> Parser.number_to_float num |> Infseq.repeat
    | Stream (FinStream sq) -> Infseq.cycleSq sq
    | Stream (InfStream sq) -> sq
    | _ -> Infseq.repeat 0.0
  in
  let f input =
    match find_type lst FloatList with
    | FloatList -> input |> List.map make_float |> fun xs -> Ok (FloatLst xs)
    | InfList ->
        input |> List.map make_infinite_sq |> fun xs -> Ok (InfStreamLst xs)
    | FinList ->
        input |> List.map make_finite_sq |> fun xs -> Ok (FinStreamLst xs)
    | Impossible mistake -> Error (Problem mistake)
  in
  f lst

let traverse_map_option f lst =
  (* map a function that returns an option over a list, if any result is None, short-circuit.*)
  let rec aux f list acc =
    match list with
    | head :: tail -> (
        match f head with Some a -> aux f tail (a :: acc) | None -> None)
    | [] -> Some (List.rev acc)
  in
  aux f lst []

let traverse_map_result f lst =
  (* same as option, but than for result, on first error fail *)
  let rec aux f list acc =
    match list with
    | head :: tail -> (
        match f head with
        | Result.Ok a -> aux f tail (a :: acc)
        | Error e -> Error e)
    | [] -> Result.Ok (List.rev acc)
  in
  aux f lst []

let binaryOp floatOp intOp lst =
  let append_floats e1 e2 =
    match (e1, e2) with
    | Constant (Float f1), Constant (Float f2) ->
        Ok (Constant (Float (floatOp f1 f2)))
    | Constant (Float _), exp ->
        Error (expression_to_string exp ^ " is not a Float" |> problemize)
    | exp, _ -> Error (expression_to_string exp ^ "is not a Float" |> problemize)
  in
  let append_ints e1 e2 =
    match (e1, e2) with
    | Constant (Integer i1), Constant (Integer i2) ->
        Ok (Constant (Integer (intOp i1 i2)))
    | Constant (Integer _), exp ->
        Error (expression_to_string exp ^ " is not an Integer" |> problemize)
    | exp, _ ->
        Error (expression_to_string exp ^ "is not a Integer" |> problemize)
  in
  let rec sum_nums summer start rest =
    match rest with
    | [] -> start
    | e1 :: tail -> (
        match start with
        | Result.Error err -> Result.Error err
        | Result.Ok e2 -> sum_nums summer (summer e1 e2) tail)
  in
  (* finally I can use a monoid !!! zero is the mempty for addition of integers and floats !!!!! *)
  match lst with
  | [] -> Ok (Constant (Integer 0))
  | Constant (Integer i1) :: rest ->
      sum_nums append_ints (Ok (Constant (Integer i1))) rest
  | Constant (Float f1) :: rest ->
      sum_nums append_floats (Ok (Constant (Float f1))) rest
  | exp :: _ ->
      Error
        (expression_to_string exp ^ "plus cannot apply, not a number"
        |> problemize)

let plus lst = binaryOp ( +. ) ( + ) lst
let minus lst = binaryOp ( -. ) ( - ) lst
let divide lst = binaryOp ( /. ) ( / ) lst
let multiply lst = binaryOp ( *. ) ( * ) lst
let nil = List []

let handle_not lst =
  match lst with
  | [ List [] ] -> Ok TrueExpression
  | [ _ ] -> Ok nil
  | _ -> Error (Problem "not takes one argument")

let compare_op flt_op int_op lst =
  let test bool = if bool then TrueExpression else nil in
  match lst with
  | [ Constant (Float f1); Constant (Float f2) ] -> Ok (test (flt_op f1 f2))
  | [ Constant (Integer i1); Constant (Integer i2) ] -> Ok (test (int_op i1 i2))
  | [ Constant (Float f1); Constant (Integer i2) ] ->
      Ok (test (flt_op f1 (float_of_int i2)))
  | [ Constant (Integer i1); Constant (Float f2) ] ->
      Ok (test (flt_op (float_of_int i1) f2))
  | [ _; _ ] -> Error (Problem "expected numbers")
  | _ -> Error (Problem "wrong number of arguments")

let bigger_than lst = compare_op ( > ) ( > ) lst
let smaller_than lst = compare_op ( < ) ( < ) lst
let bigger_or_equal_to lst = compare_op ( >= ) ( >= ) lst
let smaller_or_equal_to lst = compare_op ( <= ) ( <= ) lst
let equal lst = compare_op ( == ) ( == ) lst

let cons lst =
  match lst with
  | [ a; List b ] -> Ok (List (a :: b))
  | _ -> Error (Problem "Invalid types of arguments for cons")

let car lst =
  match lst with
  | [ List (head :: _) ] -> Ok head
  | [ List [] ] -> Error (Problem "car is not for an empty list")
  | _ -> Error (Problem "car is for a list")

let cdr lst =
  match lst with
  | [ List (_ :: tail) ] -> Ok (List tail)
  | [ List [] ] -> Error (Problem "cdr is not for an empty list")
  | _ -> Error (Problem "cdr is for non-empty list")

let list lst = Ok (List lst)
let is_list lst = match lst with [ List _ ] -> Ok TrueExpression | _ -> Ok nil

(* these are internal! *)
let is_symbol exp = match exp with Symbol _ -> true | _ -> false
let is_function exp = match exp with Function _ -> true | _ -> false

let is_symbol_quip lst =
  match lst with [ Symbol _ ] -> Ok TrueExpression | _ -> Ok nil

let is_function_quip lst =
  match lst with Function _ :: _ -> Ok TrueExpression | _ -> Ok nil

let length lst =
  match lst with
  | [ List list ] -> Ok (Constant (Integer (List.length list)))
  | _ -> Error (Problem "Invalid argument for length")

(* ************** * * * * *********** **** ** * * * * ***** * *  * * * * * * * ** ** * * * * * * * *** * * * * * * ** * * ** * * *
   Cisp functions
*)

let result_and_then f res = Result.bind res f

type sequence = FloatSq of float List.t | StreamSq of stream List.t

(* if all floats, float list
   if at least one stream, streams
    if one float other streams streams
      if all infinite, infinite stream *)

let mkFloat = function
  | Constant n -> n |> Parser.number_to_float |> Result.ok
  | _ -> Result.Error (Problem "seq requires numbers only")

let seq_old lst =
  (* this is normal seq *)
  lst
  |> traverse_map_result mkFloat
  |> Result.map (fun flt_lst -> Stream (InfStream (Infseq.seq flt_lst)))

let fin_stream_seq (lst : float Seq.t list) =
  lst |> List.to_seq |> Cisp.transpose |> Cisp.concat

let inf_stream_seq (lst : float Infseq.t list) =
  lst |> List.to_seq |> Infseq.transpose |> Infseq.concatSq

let fin_stream_cycle (lst : float Seq.t list) =
  lst |> Infseq.seq |> Infseq.concatSq

let seq lst =
  lst |> monoform
  |> Result.map (fun mono_lst ->
         match mono_lst with
         | FloatLst flt_lst -> Stream (FinStream (List.to_seq flt_lst))
         | FinStreamLst fin_str_lst ->
             Stream (FinStream (fin_stream_seq fin_str_lst))
         | InfStreamLst inf_stream_list ->
             Stream (InfStream (inf_stream_seq inf_stream_list)))

let cycle lst =
  lst |> monoform
  |> result_and_then (fun mono_lst ->
         match mono_lst with
         | FloatLst flt_lst -> Ok (Stream (InfStream (Infseq.seq flt_lst)))
         | FinStreamLst fin_str_lst ->
             Ok (Stream (InfStream (fin_stream_cycle fin_str_lst)))
         | InfStreamLst _ ->
             Error (Problem "Infite cycle on infinite seq is not very sensible"))

let st lst =
  match lst with
  | [] -> Result.Error (Problem "st [] makes no sense")
  | [ Stream (FinStream seq) ] ->
      Stream (FinStreams (Infseq.repeat seq)) |> Result.ok
  | [ Constant value ] ->
      value |> Parser.number_to_float
      |> (fun flt -> Stream (InfStream (Infseq.repeat flt)))
      |> Result.ok
  | _ -> Result.Error (Problem "st cannot accept this argument")

let ch lst =
  let handle_lst lst =
    match lst with
    | FloatLst flst -> Cisp.ch (Array.of_list flst) |> fin_stream
    | InfStreamLst inf_stream_lst ->
        inf_stream_lst |> Array.of_list |> Infseq.ch_seq |> inf_stream
    | FinStreamLst fin_stream_lst ->
        fin_stream_lst |> Array.of_list |> Cisp.choice_seq |> fin_stream
  in
  lst |> monoform |> Result.map handle_lst

let take lst =
  match lst with
  | [ Constant number; Stream str ] -> (
      let n = number |> number_to_float |> truncate in
      match str with
      | InfStream inf_str ->
          Result.ok (Stream (FinStream (Infseq.take n inf_str)))
      | FinStream fin_str ->
          Result.ok (Stream (FinStream (Cisp.take n fin_str)))
      | FinStreams _ -> Result.error (problemize "todo take for finstreams")
      | FinFinStreams _ ->
          Result.error (problemize "todo take for finfinstream"))
  | _ -> Result.error (problemize "\"take\", takes a number and a stream")

let transcat lst =
  let handle_fin_stream lst =
    lst |> List.to_seq |> Seq.transpose |> Seq.concat |> fun catted ->
    Ok (Stream (FinStream catted))
  in
  let handle_inf_stream lst =
    lst |> List.to_seq |> Infseq.transpose |> Infseq.concatSq |> fun catted ->
    Ok (Stream (InfStream catted))
  in
  lst |> monoform
  |> result_and_then (fun mono_lst ->
         match mono_lst with
         | FloatLst flt_lst -> Ok (Stream (InfStream (Infseq.seq flt_lst)))
         | FinStreamLst fin_str_lst -> handle_fin_stream fin_str_lst
         | InfStreamLst inf_str_lst -> handle_inf_stream inf_str_lst)

let lst_of_two_streams lst =
  let st a = InfStream (Infseq.repeat (Parser.number_to_float a)) in
  match lst with
  | [ Stream a; Stream b ] -> Ok (a, b)
  | [ Constant a; Constant b ] -> Ok (st a, st b)
  | [ Stream a; Constant b ] -> Ok (a, st b)
  | [ Constant a; Stream b ] -> Ok (st a, b)
  | _ -> Error (Problem "I expected two streams")

let lst_of_two_streams_finite lst =
  let st a = FinStream (Seq.return (Parser.number_to_float a)) in
  match lst with
  | [ Stream a; Stream b ] -> Ok (a, b)
  | [ Constant a; Constant b ] -> Ok (st a, st b)
  | [ Stream a; Constant b ] -> Ok (a, st b)
  | [ Constant a; Stream b ] -> Ok (st a, b)
  | _ -> Error (Problem "I expected two streams")

let hold lst =
  let hold_stream two_streams =
    match two_streams with
    | InfStream repeats, InfStream source ->
        Ok (InfStream (Infseq.hold (Infseq.map int_of_float repeats) source))
    | InfStream repeats, FinStream source ->
        let finite_repeats = Infseq.to_seq repeats |> Cisp.intify in
        Ok (FinStream (Cisp.hold finite_repeats source))
    | FinStream repeats, InfStream source ->
        let finite_source = Infseq.to_seq source in
        Ok (FinStream (Cisp.hold (repeats |> Cisp.intify) finite_source))
    | FinStream repeats, FinStream source ->
        Ok (FinStream (Cisp.hold (Cisp.intify repeats) source))
    | _ -> Error (Problem "Hold does not support streams of streams as args")
  in
  lst |> lst_of_two_streams |> result_and_then hold_stream |> Result.map stream

let binary_sq_function inf_func finite_func two_streams =
  match two_streams with
  | InfStream a, InfStream b -> Ok (InfStream (inf_func a b))
  | InfStream a_inf, FinStream b ->
      let a_fin = Infseq.to_seq a_inf in
      Ok (FinStream (finite_func a_fin b))
  | FinStream a, InfStream b ->
      Ok (FinStream (finite_func a (b |> Infseq.to_seq)))
  | FinStream a, FinStream b -> Ok (FinStream (finite_func a b))
  | _ ->
      Error
        (Problem "this function does not support streams of streams as args")

let rv lst =
  let rv_fun =
    let inf_func = Infseq.map2 Toolkit.rvfi in
    binary_sq_function inf_func Cisp.rvf
  in
  lst |> lst_of_two_streams |> result_and_then rv_fun |> Result.map stream

let result_alternative f1 f2 value =
  let r1 = f1 value in
  match r1 with Ok r1 -> Ok r1 | Error _ -> f2 value

let fold_quiplist_zip operator empty_seq empty_inf_seq (lst : quiplist) =
  match lst with
  | FloatLst flt_lst ->
      Constant (Parser.Float (List.fold_left operator 0.0 flt_lst))
      (* List.fold_left *)
  | FinStreamLst fin_str_lst ->
      Stream
        (FinStream
           (List.fold_left (Cisp.zipWith operator) empty_seq fin_str_lst))
  | InfStreamLst inf_str_lst ->
      Stream
        (InfStream
           (List.fold_left (Infseq.zipWith operator) empty_inf_seq inf_str_lst))

let fold_quiplist_cartesian operator (lst : quiplist) =
  match lst with
  | FloatLst flt_lst ->
      Result.ok (Constant (Parser.Float (List.fold_left operator 0.0 flt_lst)))
  | FinStreamLst fin_str_lst ->
      Result.ok
        (Stream
           (FinStream
              (List.fold_left (Cisp.cartesian operator) (Seq.return 0.0)
                 fin_str_lst)))
  | InfStreamLst _ ->
      Result.Error
        (Problem "You probably do not want to cartesian map the infinite")

let op_cartesian op lst =
  let mono = lst |> monoform in
  mono |> result_and_then (fold_quiplist_cartesian op)

let plus_st lst =
  match lst with
  | [ Stream (FinStream flt_a); Stream (FinStream flt_b) ] ->
      let ( <*> ) = Cisp.( <*> ) in
      Stream (FinStream (Seq.repeat ( +. ) <*> flt_a <*> flt_b)) |> Result.ok
  | [ Stream (InfStream flt_a); Stream (FinStream flt_b) ] ->
      Stream
        (InfStream
           (Infseq.repeat ( +. ) |> Infseq.andMap flt_a
           |> Infseq.andMap (Infseq.cycleSq flt_b)))
      |> Result.ok
  | [ Stream (FinStream flt_a); Stream (InfStream flt_b) ] ->
      Stream
        (InfStream
           (Infseq.repeat ( +. )
           |> Infseq.andMap (Infseq.cycleSq flt_a)
           |> Infseq.andMap flt_b))
      |> Result.ok
  | [ Stream (InfStream flt_a); Stream (InfStream flt_b) ] ->
      Stream (InfStream (Infseq.zipWith ( +. ) flt_a flt_b)) |> Result.ok
  | float_lst ->
      let plus_fun =
        binary_sq_function (Infseq.map2 ( +. )) (Cisp.map2 ( +. ))
      in
      float_lst |> lst_of_two_streams |> result_and_then plus_fun
      |> Result.map stream

let one_leg start steps = steps |> Seq.map (fun stp -> stp +. start)

let walk lst =
  let walk_fun = function
    | InfStream starts, FinStreams steps ->
        Ok (InfStream (Cisp.many_walks starts steps) |> stream)
    | FinStream starts, FinStreams steps ->
        let ws = Seq.map2 one_leg starts (Infseq.toSeq steps) |> Cisp.concat in
        Ok (Stream (FinStream ws))
    | FinStream starts, FinFinStreams steps ->
        let ws = Seq.map2 one_leg starts steps |> Cisp.concat in
        Ok (Stream (FinStream ws))
    | e1, e2 ->
        Error
          (Problem
             ("walk does not know what to do with this:\n" ^ stream_to_string e1
            ^ "-" ^ stream_to_string e2))
  in
  lst |> lst_of_two_streams_finite |> result_and_then walk_fun

(*
index lst indexer (stream)

*)
let index_inf_indexer exprs idxr =
  let int_idxr = Infseq.map (fun x -> floor x |> int_of_float) idxr in
  let handle_exprs = function
    | FloatLst lst ->
        lst |> Array.of_list |> fun arr ->
        Stream (InfStream (Infseq.index arr int_idxr)) |> Result.ok
    | _ -> Result.error (problemize "Only float list is supported")
  in
  exprs |> monoform |> result_and_then handle_exprs

let index = function
  | [ List exprs; Stream (InfStream idxr) ] -> index_inf_indexer exprs idxr
  | _ -> Result.error (problemize "index expects list of exprs and Infstream")

let line = function
  | [ Stream (InfStream targets); Stream (InfStream time) ] ->
      Ok
        (Stream
           (InfStream
              (Infseq.cycleSq
                 (Cisp.line (Infseq.to_seq targets) (Infseq.to_seq time)))))
  | _ ->
      Result.error
        (problemize "line does not support anythign else than infinite lines")

let chunks lst =
  let chunk_fun = function
    | InfStream chunk_size, InfStream input ->
        Ok (FinStreams (Infseq.chunk chunk_size input) |> stream)
    | _ -> Error (Problem "expecting infinite streams for both arguments")
  in
  lst |> lst_of_two_streams |> result_and_then chunk_fun

module Dict = Map.Make (String)

type variables = Variables of expression Dict.t

let emptyVariables = Variables Dict.empty

type environment =
  | Environment of { outer : environment option; vars : variables }

let rec env_to_string (Environment { outer; vars }) =
  let vars_to_strings (Variables vs) =
    Dict.to_seq vs
    |> Seq.map (fun (key, express) ->
           if is_function express then ""
           else key ^ "\n" ^ (express |> expression_to_string))
    |> List.of_seq |> String.concat " "
  in

  match outer with
  | Some o ->
      "Environment:\n" ^ env_to_string o ^ "\nVars:" ^ vars_to_strings vars
  | None -> vars_to_strings vars

type debug = Debug | Production

let debug_mode = Production

let debug_env context env =
  match debug_mode with
  | Debug ->
      print_string ("debug " ^ context ^ "\n");
      print_string (env_to_string env)
  | Production -> ()

let vars_from_list lst = lst |> List.to_seq |> Dict.of_seq

let initial_vars =
  [
    ("+", Function plus);
    ("-", Function minus);
    ("/", Function divide);
    ("*", Function multiply);
    ("+#", Function (op_cartesian ( +. )));
    ("-#", Function (op_cartesian ( -. )));
    ("*#", Function (op_cartesian ( *. )));
    ("/#", Function (op_cartesian ( /. )));
    ("not", Function handle_not);
    (">", Function bigger_than);
    ("<", Function smaller_than);
    (">=", Function bigger_or_equal_to);
    ("<=", Function smaller_or_equal_to);
    ("==", Function equal);
    ("car", Function car);
    ("cdr", Function cdr);
    ("cons", Function cons);
    ("list?", Function is_list);
    ("list", Function list);
    ("symbol?", Function is_symbol_quip);
    ("function?", Function is_function_quip);
    ("nil", List []);
    ("seq", Function seq);
    ("cycle", Function cycle);
    ("rv", Function rv);
    ("hold", Function hold);
    ("walk", Function walk);
    ("chunks", Function chunks);
    ("plus", Function plus_st);
    ("st", Function st);
    ("ch", Function ch);
    ("take", Function take);
    ("transcat", Function transcat);
    ("print", Function expressions_to_string);
    ("index", Function index);
    ("line", Function line) (* ("map", Function quipmap); *);
  ]
  |> vars_from_list

let initial_env = Environment { outer = None; vars = Variables initial_vars }

let rec get_var (Environment { outer; vars }) key =
  match vars with
  | Variables vars -> (
      match Dict.find_opt key vars with
      | Some exp -> Some exp
      | None -> (
          match outer with Some outer -> get_var outer key | None -> None))

let set_var (Environment { outer; vars }) key value =
  match vars with
  | Variables variables ->
      Environment { outer; vars = Variables (Dict.add key value variables) }

let parseAtoms = one_of_parsers
let identity x = x
let string_from_list characters = characters |> List.to_seq |> String.of_seq

let parse_symbol =
  let interesting c = (not (is_space c)) && c != ')' && c != '(' in
  parse_while interesting |> fmap string_from_list
  |> Parser.andThen (fun res ->
         match res with
         | "" -> Parser.fail_with "string to short"
         | any -> Parser.succeed any)

let atom =
  spaces
  >> one_of_parsers
       [
         string "#t" |> fmap (fun _ -> TrueExpression);
         string "nil" |> fmap (fun _ -> List []);
         number |> fmap (fun num -> Constant num);
         parse_symbol |> fmap (fun str -> Symbol str);
       ]

let expression_list =
  let rec expression_list_help rev_expressions =
    spaces
    >> one_of_parsers
         [
           char ')' |> fmap (fun _ -> Done (List (List.rev rev_expressions)));
           atom |> fmap (fun exp -> Loop (exp :: rev_expressions));
           Parser.char '('
           >> Parser.loop [] expression_list_help
           |> fmap (fun exp -> Loop (exp :: rev_expressions));
           Parser.reached_end >> Parser.fail_with (Problem "missing )");
         ]
  in
  Parser.char '(' >> Parser.loop [] expression_list_help

let symbol env var =
  match get_var env var with
  | Some exp_ -> Ok (exp_, env)
  | None -> Result.Error (Problem ("Symbol could not be found" ^ var))

let quote env lst =
  match lst with
  | [ exp ] -> Ok (exp, env)
  | _ -> Error (Problem "Quote only accepts 1 argument")

let lambda (env : environment) lst =
  debug_env "lamdba" env;
  match lst with
  | [ List vars; exp ] -> Ok (Lambda (vars, exp), env)
  | _ -> Error (Problem "Invalid lamdba expression")

let rec zip lsta lstb =
  match (lsta, lstb) with
  | ha :: ta, hb :: tb -> (ha, hb) :: zip ta tb
  | [], _ -> []
  | _, [] -> []

let params_and_args_to_vars params args =
  let to_dict_entry (par, arg) =
    match par with Symbol symb -> Some (symb, arg) | _ -> None
  in
  let opt = zip params args |> traverse_map_option to_dict_entry in
  match opt with
  | Some var_list -> Ok (vars_from_list var_list)
  | None -> Error (Problem "All arguments need to be symbols")

let rec eval env exp =
  debug_env "eval" env;
  match exp with
  | Symbol symb -> symbol env symb
  | Constant _ -> Ok (exp, env)
  | List lst -> (
      match lst with
      | Symbol "quote" :: tail -> quote env tail
      | Symbol "if" :: tail -> handle_if env tail
      | Symbol "define" :: tail -> define env tail
      | Symbol "set!" :: tail -> set env tail
      | Symbol "lambda" :: tail -> lambda env tail
      | Symbol "begin" :: tail -> begin_quip env tail
      | Symbol "" :: _ -> Error (Problem "empty symbol")
      | Symbol procName :: args -> proc env procName args
      | List [ Symbol "lambda"; List paramList; lambdaExpression ] :: args ->
          handle_lambda_execution paramList lambdaExpression env args
          |> Result.map (fun evaluated_exp -> (evaluated_exp, env))
      | [] -> Ok (nil, env)
      | _ -> Error (Problem "list should start with a proc"))
  | Function _ -> Ok (exp, env)
  | Lambda (_, _) -> Ok (exp, env)
  | TrueExpression -> Ok (exp, env)
  | Stream _ -> Ok (exp, env)

and define env exps =
  match exps with
  | [ Symbol var; exp ] -> (
      match get_var env var with
      | Some _ -> Error (Problem ("Defining already defined variable" ^ var))
      | None -> (
          match eval env exp with
          | Ok (evaledExp, newEnv) -> Ok (nil, set_var newEnv var evaledExp)
          | Error error -> Error error))
  | [ _; _ ] -> Error (Problem "First argument for define should be a symbol")
  | _ -> Error (Problem "Too many or few expressions for define")

and set env exps =
  match exps with
  | [ Symbol var; exp ] -> (
      match get_var env var with
      | Some _ -> (
          match eval env exp with
          | Ok (evaledExp, newEnv) -> Ok (nil, set_var newEnv var evaledExp)
          | Error error -> Error error)
      | None -> Error (Problem "Setting an undined variable"))
  | [ _; _ ] -> Error (Problem "First argument of set! should be a symbol")
  | _ -> Error (Problem "Wrong number of argument for set!")

and begin_quip env exps =
  match exps with
  | [] -> Ok (nil, env)
  | [ exp ] -> eval env exp
  | head :: tail -> (
      match eval env head with
      | Ok (_, newEnv) -> begin_quip newEnv tail
      | Error e -> Error e)

and proc env procName args =
  debug_env "proc" env;
  let maybe_func = get_var env procName in
  let evaled_args_result = eval_list env args in
  match (maybe_func, evaled_args_result) with
  | Some (Function func), Ok (evaledArgs, newEnv) ->
      func evaledArgs |> Result.map (fun ok -> (ok, newEnv))
  | Some (Lambda (params, exp)), Ok (evaluatedArgs, newEnv) ->
      handle_lambda_execution params exp newEnv evaluatedArgs
      |> Result.map (fun evaledExp -> (evaledExp, newEnv))
  | None, _ -> Error (Problem ("Symbol " ^ procName ^ " not found"))
  | _, Error e -> Error e
  | _, Ok _ -> Error (Problem "Not a function or lamdba expression")

and eval_list env lst =
  debug_env "eval_list" env;
  let andThen f ma = Result.bind ma f in
  match lst with
  | [] -> Ok ([], env)
  | head :: tail ->
      eval env head
      |> andThen (fun (evaledHead, newEnv) ->
             eval_list newEnv tail
             |> Result.map (fun (evaledTail, lastEnv) ->
                    (evaledHead :: evaledTail, lastEnv)))

and handle_lambda_execution params exp env args =
  debug_env "lambda exec" env;
  let andThen f ma = Result.bind ma f in
  if List.length params != List.length args then
    Error (Problem "params and args do not have same length")
  else
    let vars_result = params_and_args_to_vars params args in
    vars_result
    |> andThen (fun vars ->
           eval (Environment { vars = Variables vars; outer = Some env }) exp
           |> Result.map (fun (ret, _) -> ret))

and handle_if env lst =
  match lst with
  | [ test; conseq; alt ] -> (
      match eval env test with
      | Ok (List [], _) -> eval env alt
      | Ok _ -> eval env conseq
      | Error e -> Error e)
  | _ -> Error (Problem "If takes exactly three arguments")

let eval_string str =
  let parse_result =
    Parser.parse_str (expression_list |> Parser.fmap (eval initial_env)) str
  in
  match parse_result with
  | Parser.Good (Ok (exp, env), _) ->
      expression_to_string exp |> print_string;
      debug_env "Parser result" env (* parse_result *)
  | Parser.Good (Error (Problem problem), _) -> print_string problem
  | Parser.Problem (prob, _) ->
      Parser.problem_to_string (fun _ -> "prob") prob |> print_string

let print_stream stream =
  let print_tabbed_float str =
    str
    |> List.iter (fun x ->
           print_float x;
           print_string "\t");
    print_newline ();
    flush stdout
  in
  match stream with
  | InfStream inf_str ->
      print_endline "inf stream:";
      inf_str |> Infseq.take 40 |> Cisp.for_example |> print_tabbed_float
  | FinStream fin_str ->
      print_endline "finite stream:";
      fin_str |> Cisp.for_example |> print_tabbed_float
  | _ ->
      print_string "Sorry cannot print FinFinStreams or FinInfStreams";
      print_newline ()

let eval_string_to_stream strng =
  let parse_result =
    Parser.parse_str (expression_list |> Parser.fmap (eval initial_env)) strng
  in
  match parse_result with
  | Parser.Good (Ok (exp, _), _) -> (
      match exp with
      | Stream str -> Some str
      | _ ->
          ignore (print_string "sorry not a stream");
          None)
  | Parser.Good (Error (Problem problem), _) ->
      ignore (print_string problem);
      None
  | Parser.Problem (prob, _) ->
      Parser.problem_to_string (fun _ -> "prob") prob |> print_string |> ignore;
      None

(* utop # eval_string_to_stream "(seq 1.0 (seq 11.0 12.0) 3)";; *)

(* The question is, do we want explicit concat at the end or not
   (transcat (seq 11 12 13) (seq 1 2 3) (seq 100 200)) ->

   I think a sequence should always be of one type:
   Seq.t
   Infseq.t

   If there is a an Infseq.t it should just be converted to Seq.t, but the end result is an Infseq.t
*)

let suminfs infsqss =
  infsqss |> Seq.map Infseq.toSeq |> Seq.transpose
  |> Seq.map (Seq.fold_left ( +. ) 0.0)
  |> Infseq.cycleSq
