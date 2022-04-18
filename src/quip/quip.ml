open Parser

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
  | Function of (expression list -> (expression,problem) result)
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
          (expression_to_string exp ^ "+ cannot apply, not a number"
          |> problemize)

let plus lst =
  let append_floats e1 e2 =
    match (e1, e2) with
    | Constant (Float f1), Constant (Float f2) ->
        Ok (Constant (Float (f1 +. f2)))
    | Constant (Float _), exp ->
        Error (expression_to_string exp ^ " is not a Float" |> problemize)
    | exp, _ -> Error (expression_to_string exp ^ "is not a Float" |> problemize)
  in
  let append_ints e1 e2 =
    match (e1, e2) with
    | Constant (Integer i1), Constant (Integer i2) ->
        Ok (Constant (Integer (i1 + i2)))
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
  let res =
    (* finally I can use a monoid !!! zero is the mempty for addition of integers and floats !!!!! *)
    match lst with
    | [] -> Ok (Constant (Integer 0))
    | Constant (Integer i1) :: rest ->
        sum_nums append_ints (Ok (Constant (Integer i1))) rest
    | Constant (Float f1) :: rest ->
        sum_nums append_floats (Ok (Constant (Float f1))) rest
    | exp :: _ ->
        Error
          (expression_to_string exp ^ "+ cannot apply, not a number"
          |> problemize)
  in
  res

module Dict = Map.Make (String)

type variables = Variables of expression Dict.t

let emptyVariables = Variables Dict.empty

type environment =
  | Environment of { outer : environment option; vars : variables }

let vars_from_list lst = lst |> List.to_seq |> Dict.of_seq
let initial_vars = [ ("+", Function plus) ] |> vars_from_list
let initial_env = Environment { outer = None; vars = Variables initial_vars }

let rec get_var (Environment { outer ; vars }) key =
  match vars with
  | Variables vars ->
    match Dict.find_opt key vars with
    | Some exp -> Some exp
    | None ->
      match outer with
      | Some outer -> get_var outer key
      | None -> None

let set_var (Environment { outer ; vars } ) key value =
  match vars with
  | Variables variables ->
    Environment { outer = outer; vars = Variables (Dict.add key value variables)}




let parseAtoms =
  one_of_parsers 

let identity x = x

let string_from_list characters =
  characters |> List.to_seq |> String.of_seq 

let symbol = 
  let interesting c =
    (not (Parser.is_space c)) && c != ')' && c != '('
  in
  parse_while interesting |> fmap string_from_list

let atom =
  spaces >> one_of_parsers [
    string "#t" |> fmap (fun _ -> TrueExpression)
    ; string "nil" |> fmap (fun _ -> (List []))
    ; number |> fmap (fun num -> (Constant num)) 
    ; symbol |> fmap (fun str -> (Symbol str))
  ]

let expression_list =
  let expression_list_help rev_expressions =
    one_of_parsers [
      char ')' |> fmap (fun _ -> Done (List.rev rev_expressions))
      ; atom |> fmap (fun exp -> (Loop (exp :: rev_expressions)))
    ]
  in
  char '(' 
    >> Parser.loop [] expression_list_help



let symbol env var =
  match get_var env var with
  | Some exp_ -> Ok ( exp_, env )
  | None -> Result.Error (Problem ("Symbol could not be found" ^ var))

let quote env lst =
  match lst with
  | [ exp ] -> Ok (exp, env)
  | _ -> Error (Problem "Quote only accepts 1 argumnet")

let nil = 
  List []

let lambda (env:environment) lst =
  match lst with
  | [ List vars ; exp ] -> Ok ( Lambda (vars,exp), env )
  | _ -> Error (Problem "Invalid lamdba expression") 


    


let rec eval env exp =
  match exp with
  | Symbol symb -> symbol env symb 
  | Constant const -> Ok (exp, env)
  | List lst ->
    (
    match lst with
      | (Symbol "quote") :: tail -> quote env tail 
  (*  | (Symbol "if" :: tail -> )*)
      | (Symbol "define") :: tail -> define env tail
      | (Symbol "set!") :: tail -> set env tail
      | (Symbol "lambda") :: tail -> lambda env tail
      | (Symbol "begin") :: tail -> begin_quip env tail 
      | (Symbol "" :: tail) -> Error (Problem "empty symbol")
      | Symbol procName :: tail -> proc env procName args 
      | (List [ Symbol "lambda"; List paramList; lambdaExpression ]) :: args ->
        (
        match handleLambdaExecution paramList lambdaExpression env args with
          | Ok evaluatedExpression -> Ok (evaluatedExpression , env) 
          | Error e -> Error e
        )
      | [] -> Ok (nil, env)
      | _ -> Error (Problem "list should start with a proc" )
    )
  | Function _ -> Ok (exp, env)
  | Lambda (_,_) -> Ok (exp, env)
  | TrueExpression -> Ok (exp, env)
  and define env exps =
    match exps with 
    | [ Symbol var; exp ] -> 
      (
      match get_var env var with
        | Some _ -> Error (Problem ("Defining already defined variable" ^ var))
        | None -> match eval env exp with
          | Ok (evaledExp, newEnv) -> Ok ( nil, set_var newEnv var evaledExp)
          | Error error -> Error error
      )
  
    | [_; exp] ->
      Error (Problem "First argument for define should be a symbol")
  
    |  _ ->
        Error (Problem "Too many or few expressions for define") 
  and set env exps =
    match exps with
    | [ Symbol var; exp ] -> 
      (
      match get_var env var with
      | Some _ -> 
        (
        match eval env exp with
        |  Ok (evaledExp, newEnv ) -> Ok (nil, set_var newEnv var evaledExp )
        | Error error -> Error error
        )
      | None -> Error (Problem "Setting an undined variable")
      )
    | [ _ ; exp ] -> Error (Problem "First argument of set! should be a symbol")
    | _ -> Error (Problem "Wrong number of argument for set!") 
  and begin_quip env exps =
    match exps with
      | [] -> Ok (nil, env)
      | [ exp ] -> eval env exp 
      | head :: tail -> 
        (
          match eval env head with
            | Ok ( _, newEnv ) -> begin_quip newEnv tail 
            | Error e -> Error e
        )
  and proc env procName args =
    let maybe_func = get_var env procName in
    let evaled_args_result = eval_list env args  in
    match (maybe_func, evaled_args_result) with
      | (Some (Function func), Ok ( evaledArgs, newEnv)) -> 
        (match func evaledArgs with
          | Ok ok -> Ok (ok, newEnv)
          | Error e -> Error e)
      | (Some (Lambda (params,exp)), Ok (evaluatedArgs, newEnv) ) -> 
        (match handleLambdaExecution params exp newEnv evaluatedArgs with
          | Ok (evaledExp, newEnv)  ->
          | Error e -> Error e)
      | ( None, _ ) -> Error (Problem ("Symbol " ^ procName ^ " not found"))
      | ( _, Error e ) -> Error e
      | (_, Ok _) -> Error (Problem "Not a function or lamdba expression")
  and eval_list env lst =
    match lst with
      | [] -> Ok ( [] , env)
      | head :: tail ->
        match eval env head with
          | Ok (evaledHead, newEnv ) ->
            (
            match eval_list newEnv tail with
              | Ok (evaledTail, lastEnv ) -> Ok (evaledHead :: evaledTail, lastEnv)
              | Error e -> Error e
            )
          | Error e -> Error e
  and handleLambdaExecution params exp env args =
    if List.length params != List.length args then
        Error (Problem "params and args do not have same length")
    else
        let varsResult = paramsAndArgsToVars params args in
        match varsResult with
          | Ok vars ->
            (
              match eval (Environment { vars = vars ; outer = Some env }) exp with
                | Ok ( ret, _ ) -> Ok ret
                | Error e -> Error e
            )
          |  Err error ->
              Err error


                


    
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
