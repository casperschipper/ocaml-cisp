open Cisp
open Seq


let timer = st 0.25

let values = seq [11;12;13] |> map Int.to_float

let timer2 = st 0.25

let values2 = seq [11;12;13] |> map Int.to_float

let () =
  let threads = Lwt.join [
       ( OscStream.play "127.0.0.1" 56789 "/par1" timer values );
       ( OscStream.play "127.0.0.1" 56789 "/par2" timer2 values2 )]
  in
  Lwt_main.run threads



