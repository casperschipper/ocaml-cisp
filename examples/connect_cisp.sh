dune exec ./cisp.exe &
sleep 1
jack_connect "system_midi:capture_2" "ocaml midi client:ocaml_midi_in" &
