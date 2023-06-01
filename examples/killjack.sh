#!/bin/sh
echo "kill jack !"
pgrep jackd | echo
pgrep jackd | xargs kill -s KILL
