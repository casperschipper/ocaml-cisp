external caml_snd_write :  
  (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t 
  ->  string -> int * int * int * int -> unit = "caml_snd_write"
    
external caml_snd_read :  
  string ->
  (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t 
  = "caml_snd_read"

external caml_n_channels :  
  string -> int 
 = "caml_n_channels"

external caml_n_samplerate :  
  string -> int 
 = "caml_n_samplerate"
    
type snd_format =
  | WAV_8
  | WAV_16
  | WAV_24
  | WAV_32
  | WAV_FLOAT
  | AIFF_8
  | AIFF_16
  | AIFF_24
  | AIFF_32
  | AIFF_FLOAT
  | RAW_8
  | RAW_16
  | RAW_24
  | RAW_32
  | RAW_FLOAT
      
let make_format_tuple sr chns = function
  | WAV_8 -> (sr,chns,0x0001,0x010000)
  | WAV_16 -> (sr,chns,0x0002,0x010000)
  | WAV_24 -> (sr,chns,0x0003,0x010000)
  | WAV_32 -> (sr,chns,0x0004,0x010000)
  | WAV_FLOAT -> (sr,chns,0x0006,0x010000)
  | AIFF_8 -> (sr,chns,0x0001,0x020000)
  | AIFF_16 -> (sr,chns,0x0002,0x020000)
  | AIFF_24 -> (sr,chns,0x0003,0x020000)
  | AIFF_32 -> (sr,chns,0x0004,0x020000)
  | AIFF_FLOAT -> (sr,chns,0x0006,0x020000)
  | RAW_8 -> (sr,chns,0x0001,0x040000)
  | RAW_16 -> (sr,chns,0x0002,0x040000)
  | RAW_24 -> (sr,chns,0x0003,0x040000)
  | RAW_32 -> (sr,chns,0x0004,0x040000)
  | RAW_FLOAT -> (sr,chns,0x0006,0x040000)
      
      
let snd_write array string sr chns format =
  caml_snd_write array string (make_format_tuple sr chns format)
    
let snd_read = caml_snd_read

let snd_channels = caml_n_channels

let snd_sr = caml_n_samplerate
  

