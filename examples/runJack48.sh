#!/bin/bash

jackd -X coremidi -d coreaudio -r 48000 &
echo $!
