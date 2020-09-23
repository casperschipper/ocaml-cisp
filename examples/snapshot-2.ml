open Stdlib
open Printf

(* 

https://scylardor.fr/2013/10/14/ocaml-parsing-a-programs-arguments-with-the-arg-module/

snapshot -save name 
snapshot -save name -load newName 
snapshot -overwrite name (to be implemented)

*)

let snapshotFolder =
  "cisp_backup/"

let cispCopyCmd number targetFolder =
  let cispName = "cisp" ^ Int.to_string number ^ ".ml" in
  "cp " ^ cispName ^ " " ^ snapshotFolder ^ targetFolder ^ "/" ^ cispName

let cispLoadCmd number targetFolder =
  let cispName = "cisp" ^ Int.to_string number ^ ".ml" in
  "cp " ^ " cisp_backup/" ^ targetFolder ^ "/" ^ cispName ^ " " ^ cispName

let makeFolderCmd folderName =
  "mkdir " ^ snapshotFolder ^folderName

let testCommand command =
  command ^ "\n" |>
    print_endline

let runCommand command =
  Sys.command command

let testDir name =
  "test -d " ^ "\"" ^ snapshotFolder ^ name ^ "\""

let isNewDir dir =
   testDir dir |> runCommand |> ((==) 1)


let (--) i j = 
    let rec aux n acc =
      if n < i then acc else aux (n-1) (n :: acc)
    in aux j []


let saveSnapshot name =
  let _ = makeFolderCmd name |> runCommand in
  let nums = 1--8 in
  let results = List.map (fun idx ->
                    cispCopyCmd idx name |> runCommand 
                  ) nums
  in
  if List.for_all ((!=) 0) results
  then print_endline "failure!"
  else  "files have been backed up to: " ^ name ^ "\n" |> print_endline

let loadSnapshot name = 
  let nums = 1--8 in
  let results = List.map (fun idx ->
                    cispLoadCmd idx name |> runCommand
                  ) nums
  in
  if List.for_all ((!=) 0) results
  then print_endline "something failed!"
  else "files have been loaded!" ^ name ^ "\n" |> print_endline

let trySave name =
  if isNewDir name then
    saveSnapshot name 
  else
    print_endline ("this name exists!" ^ name)


let tryLoad name =
  if isNewDir name then
    print_endline ("there is no snapshot by that name: " ^ name)
  else
    loadSnapshot name
    
    


let () =
  let specList = [("-save", Arg.String trySave, "[snapshot name] : Saves current cisp files to folder with provided unique name");
                  ("-load", Arg.String tryLoad, "[snapshot name] : Loads the cisp files from directory into your folder")
                 ] in
  let usage_msg = "This is a snapshot creater and loader. It copies all the cisp1 till cisp8 from a subfolder" in
  Arg.parse specList print_endline usage_msg
  
    
   
 

