#!/bin/bash

jackd -R -X coremidi -d coreaudio -P BlackHole16ch_UID -C BlackHole16ch_UID -r 44100 -p 1024 &
echo $!
