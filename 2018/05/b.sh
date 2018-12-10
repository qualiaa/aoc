#!/bin/bash

input=$(cat -)
for a in {a..z}; do tr -d "$a${a^^}"<<<"$input" | ./a.sh; done | sort -n | head -1
