#!/bin/sh

input=$(cat -)
{ tr -d '\n' <<<"$input"; echo; } | bc
yes -- "$input" | awk '{ fs[f] = 1; f += $0; if (f in fs) { print f; exit } }'
