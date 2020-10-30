#!/bin/bash
dune exec ./decoherence.exe &
sleep 3
echo "ok start connecting"
jack_lsp
jack_connect system:capture_1 ocaml:input_0
jack_connect system:capture_1 ocaml:input_1
jack_connect system:capture_1 ocaml:input_2
jack_connect system:capture_1 ocaml:input_3
jack_connect system:capture_1 ocaml:input_4
jack_connect system:capture_1 ocaml:input_5
jack_connect system:capture_1 ocaml:input_6
jack_connect system:capture_1 ocaml:input_7
