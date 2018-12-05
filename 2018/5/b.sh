#!/bin/bash

input=$(cat -)
for a in {a..z}; do
    tr -d "$a${a^^}"<<<"$input" |\
    sed -Ef <(echo "::";for a in {a..z}; do A=${a^^};echo "s/($a$A|$A$a)//g;t:"; done) |\
    tr -d '\n' | wc -m
done | sort -n | head -1
