#!/bin/bash
dune exec ./decoherence.exe &
sleep 3
echo "ok start connecting"
jack_lsp
jack_connect ocaml:output0 ffmpeg_out_0:input_1
jack_connect ocaml:output1 ffmpeg_out_0:input_2
jack_connect ocaml:output0 icecast:left
jack_connect ocaml:output1 puredata:output1 icecast:right                                                                                                            
jack_connect klangraum_input:out_1 ocaml:input_0
jack_connect klangraum_input:out_2 ocaml:input_1                                                                                                  
jack_connect klangraum_input:out_3 ocaml:input_2                                                                                                  
jack_connect klangraum_input:out_4 ocaml:input_3                                                                                                  
jack_connect klangraum_input:out_5 ocaml:input_4                                                                                                  
jack_connect klangraum_input:out_6 ocaml:input_5                                                                                                  
jack_connect klangraum_input:out_7 ocaml:input_6                                                                                                  
jack_connect klangraum_input:out_8 ocaml:input_7                                                                                                  
jack_connect klangraum_input:out_9 ocaml:input_0                                                                                                  
jack_connect klangraum_input:out_10 ocaml:input_1                                                                                              
jack_connect klangraum_input:out_11 ocaml:input_2                                                                                                
jack_connect klangraum_input:out_12 ocaml:input_3                                                                                                
jack_connect klangraum_input:out_13 ocaml:input_4                                                                                                
jack_connect klangraum_input:out_14 ocaml:input_5                                                                                                
jack_connect klangraum_input:out_15 ocaml:input_6                                                                                                
jack_connect klangraum_input:out_16 ocaml:input_7                                                                                                
jack_connect klangraum_input:out_17 ocaml:input_0                                                                                                
jack_connect klangraum_input:out_18 ocaml:input_1                                                                                                
1jack_connect klangraum_input:out_19 ocaml:input_2                                                                                                
jack_connect klangraum_input:out_20 ocaml:input_3                                                                                                
jack_connect klangraum_input:out_21 ocaml:input_4                                                                                                
jack_connect klangraum_input:out_22 ocaml:input_5                                                                                                
jack_connect klangraum_input:out_23 ocaml:input_6                                                                                                
jack_connect klangraum_input:out_24 ocaml:input_7                                                                                                
jack_connect klangraum_input:out_25 ocaml:input_0                                                                                                
jack_connect klangraum_input:out_26 ocaml:input_1                                                                                                
jack_connect klangraum_input:out_27 ocaml:input_2                                                                                                
jack_connect klangraum_input:out_28 ocaml:input_3
