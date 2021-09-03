#!/bin/bash

jackd -X coremidi -d coreaudio -r 44100 -p 1024 &
echo $!
