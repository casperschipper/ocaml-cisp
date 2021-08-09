#!/bin/bash

jackd -X coremidi -d coreaudio

sleep 8

jack_lsp -A

jack_connect system_midi:capture_2 "ocaml midi client:ocaml_midi_in"
jack_connect "ocaml midi client:ocaml_midi_out" system_midi:playback_1

sleep 1.0

jack_lsp -c
