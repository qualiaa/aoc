#!/bin/bash

input=$(cat -)

state=$(head -1 <<<$input | tr -cd '#.' | tr . %)
length=$(($(wc -m <<<$state) - 1))

function growth_script() {
    cat <<SCRIPT
:: s/^/\n/;:n
$(tail -n+2 <<<$input | tr . % | sed -nE 's|([#%]{5}) => #|/\\n\1/ba|p')
s/\n/%&/;bf;:a;s/\n/#&/;bf
:f;s/\n./\n/;/\n[#%]{5}/bn;P;s/\n.*$//;/(^#|#$)/s/^.*$/%&%/;s/^.*$/%%&%%/;b:
SCRIPT
}

function pad {
    printf "%.s%%" $(seq $1); printf "%s" "$2"; printf "%.s%%" $(seq $1); echo
}

function iterate() {
    sed -nEf <(growth_script) <<<$(pad 2 $state)
}

function find_stable_states()
{
    sed -nE 'N;h;s/^%+#/#/;s/\n%+#/\n#/;::;s/^(.)(.*)\n\1/\2\n/;t:;/#/!{=;g;p;q};g;D'
}

function sum_list() {
    paste -sd+ | bc
}

function get_offset() {
    local state=$1
    local l=$(($(wc -m <<<$state) - 1))
    echo $(( (length - l - 2)/2 ))
}

function score_flowers() {
    local state=$(cat -)
    local offset=$(get_offset $state)
    grep -o . <<<$state |sed = | sed "N;/%/d;s/\n.*/$offset/"  | bc | sum_list
}

iterate | tail +20 | head -1 | score_flowers

stable_states=($(iterate | find_stable_states))

iteration=${stable_states[0]}

score1=$(score_flowers<<<${stable_states[1]})
score2=$(score_flowers<<<${stable_states[2]})

((scoreDiff = score2 - score1))
((timeRemaining = 50000000000 - iteration))
((finalScore = scoreDiff * timeRemaining + score2))

echo $finalScore
