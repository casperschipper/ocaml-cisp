#!/bin/bash

jack_connect system_midi:capture_2 "ocaml midi client:ocaml_midi_in"
jack_connect "ocaml midi client:ocaml_midi_out" system_midi:playback_1
echo "made connections"
jack_lsp -A
