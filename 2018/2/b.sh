#!/bin/bash

readonly lines=$(cat -)

function after() {
    sed "0,/$1/d" <<< "$lines"
}

function v () {
    grep -o . <<<$1
}

# soln 1:
# diff is about twice as fast but will fail in cases of rotation.
# abcdef, bcdefa  reports one - and one +
for lineA in $lines; do
    for lineB in $(after $lineA); do
        diff -U 100 <(v $lineA) <(v $lineB) | tail -n+4 | tr -d '\n ' |\
            sed '/-.*-/d;s/[+-].//g;s/$/\n/'
    done
done

# soln 2:
# this method is slow but robust
for lineA in $lines; do
    for lineB in $(after $lineA); do
        paste <(v $lineA) <(v $lineB) | sed -E '/(.)\t\1/b;c#'  | cut -c1 |\
           tr -d '\n' | grep -v '#.*#' | tr -d '#'
    done
done

# soln 3:
# robust method almost entirely in sed
for lineA in $lines; do
    for lineB in $(after $lineA); do
        paste <(v $lineA) <(v $lineB) |\
           sed -E '::;s/(.)\t\1$/\1/;s/.\t.$/#/;/#.*#/Q;N;s/\n//;b:' | tr -d '#'
    done
done
