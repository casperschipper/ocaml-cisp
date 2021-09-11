#!/bin/bash

jackd -R -X coremidi -d coreaudio -r 44100 -p 4096 &
echo $!
