#!/bin/bash

sed -Ef <(echo "::";for a in {a..z}; do A=${a^^};echo "s/($a$A|$A$a)//g;t:"; done) |\
    tr -d '\n' | wc -m
