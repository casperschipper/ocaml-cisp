#!/bin/bash

jackd -R -X coremidi -d coreaudio --device "Babyface (23647366)"  -r 44100 -p 1024 &
echo $!
