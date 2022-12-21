#!/bin/bash

jackd -R -X coremidi -d coreaudio -r 48000 -p 1024 &
echo $!
