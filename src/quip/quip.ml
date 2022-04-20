open Parser

(*
   mainly a port of
   tomotakatakahashi's https://github.com/tomotakatakahashi/lispy-elm
   which in turn is based on
   http://www.norvig.com/lispy.html
*)

let assert_equal label a b =
  if a = b then print_string "all ok"
  else print_string ("failed: " ^ label ^ " <<<should be>>> ")

type problem = Problem of string

let problemize p = Problem p

type expression =
  | Constant of Parser.number
  | Symbol of string
  | List of expression list
  | Lambda of expression list * expression
  | Function of (expression list -> (expression, problem) result)
  | TrueExpression

let rec expression_to_string exp =
  match exp with
  | Constant n -> Parser.number_to_string n
  | Symbol str -> str
  | List lst ->
      "(" ^ (lst |> List.map expression_to_string |> String.concat " ") ^ ")"
  | Lambda (lst, body) ->
      "lambda "
      ^ expression_to_string (List lst)
      ^ " " ^ expression_to_string body
  | Function _ -> "function"
  | TrueExpression -> "#t"

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
        (expression_to_string exp ^ "+ cannot apply, not a number" |> problemize)

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

let is_list lst =
  match lst with [ List _ ] -> Ok TrueExpression | _ -> Ok nil

let is_symbol exp = 
  match exp with Symbol _ -> true | _ -> false

let is_symbol_quip lst =
  match lst with
  | [ Symbol _ ] -> Ok TrueExpression
  | _ -> Ok nil

let length lst =
  match lst with
  | [ List list ] -> Ok (Constant (Integer (List.length list)))
  | _ -> Error (Problem "Invalid argument for length")

module Dict = Map.Make (String)

type variables = Variables of expression Dict.t

let emptyVariables = Variables Dict.empty

type environment =
  | Environment of { outer : environment option; vars : variables }

let vars_from_list lst = lst |> List.to_seq |> Dict.of_seq

let initial_vars =
  [
    ("+", Function plus);
    ("-", Function minus);
    ("/", Function divide);
    ("*", Function multiply);
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
    one_of_parsers
      [
        char ')' |> fmap (fun _ -> Done (List (List.rev rev_expressions)));
        atom |> fmap (fun exp -> Loop (exp :: rev_expressions));
        spaces >> Parser.char '('
        >> Parser.loop [] expression_list_help
        |> fmap (fun exp -> Loop (exp :: rev_expressions));
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
  | _ -> Error (Problem "Quote only accepts 1 argumnet")

let lambda (env : environment) lst =
  match lst with
  | [ List vars; exp ] -> Ok (Lambda (vars, exp), env)
  | _ -> Error (Problem "Invalid lamdba expression")


let traverse_map_option f lst =
  let rec aux f list acc =
    match list with
    | head :: tail -> (
        match f head with Some a -> aux f tail (a :: acc) | None -> None)
    | [] -> Some (List.rev acc)
  in
  aux f lst []

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
  Parser.parse_str (expression_list |> Parser.fmap (eval initial_env)) str

(*
   let quip =
       one_of_parsers
       [
         Parser.number
         ; expression_list
       ] *)

(*

   eval (seq 1 2 3)

   ->

   list [Symbol Seq;Number (Int 1);Number (Int 2);Number (Int 3)]

   (InfSeq.seq [1.0;2.0;3.0])
*)

(* (2 3 4 5 6) *)
(* (/ 1 2 3 4) *)

(*

   (seq 11 12 13) -> seq int int int

   (seq 11 (rv 1 10) 13) -> (seq 11 (st 11) (rv 1 10) 13)

   (rv 1 10) ->

   (seq (rv 1 10) 1 4)

   (count 10)
*)

(*
let rec quip_program = 
  one_of_parsers [
    Parser.float |> fmap (fun x -> Float x)
    ; Parser.natural |> fmap (fun x -> Int x)
    ; Parser. |> fmap (fun x -> String )
  ]
  *)

(* parsing = computing ! *)
