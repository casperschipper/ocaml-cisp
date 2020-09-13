open Cisp
open Seq


let timer = st 1.5

let values = seq [11;12;13] |> map Int.to_float

let () =
    Lwt_main.run ( OscStream.play "127.0.0.1" 56789 "/par1" timer values )
