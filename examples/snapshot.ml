open Stdlib
open Printf

let cispCopyCmd number targetFolder =
  let cispName = "cisp" ^ Int.to_string number ^ ".ml" in
  "cp " ^ cispName ^ " cisp_backup/" ^ targetFolder ^ "/" ^ cispName

let makeFolderCmd folderName =
  "mkdir " ^ "cisp_backup/" ^folderName

let testCommand command =
  command ^ "\n" |>
    print_string

let runCommand command =
  Sys.command command

let (--) i j = 
    let rec aux n acc =
      if n < i then acc else aux (n-1) (n :: acc)
    in aux j []

let () =
  let numberOfArg = Array.length Sys.argv - 1 in
  match numberOfArg with
  | 1 -> let targetFolder = Sys.argv.(1) in
         let _ = makeFolderCmd targetFolder |> runCommand in
         let nums = 1--8 in
         let results = List.map (fun idx ->
             cispCopyCmd idx targetFolder |> runCommand 
             ) nums
         in
         if List.for_all ((!=) 0) results
         then print_string "failure!"
         else  "files have been backed up to: " ^ targetFolder ^ "\n" |> print_string        
  | _ -> print_string "no spaces allowed in name"
    
   
 

