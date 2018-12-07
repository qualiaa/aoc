#!/bin/bash

cat - > tasks
for a in {A..Z}; do
    if cut -c2- tasks | grep -q "$a"; then echo $a; fi
done > letters

while [ -s letters ]; do
    for a in $(cat letters); do
        if ! grep -q "$a can" tasks; then
            sed -i "/$a must/d" tasks
            sed -i "/$a/d" letters
            echo -n $a
            break
        fi
    done
done
echo
rm letters tasks
