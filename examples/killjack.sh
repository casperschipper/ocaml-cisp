#!/bin/sh
echo "kill jack !"
pgrep jackd | xargs kill -s KILL
