#!/bin/sh

function rotr() {
    tail -$1 <<<$2
    head -n-$1 <<<$2
}

function rotl() {
    tail +$(($1+1)) <<<$2
    head -$1 <<<$2
}

function marble_measurer() {
    local players=$1
    local marbles=$2
    local state=$(echo -e "0\n1")
    local -a scores

    for turn in $(seq 2 $marbles); do
        if (( turn % 23 )); then
            state=$(rotr 2 "$state"; echo $turn)
        else
            local player=$(( 1 + (turn-1) % players ))
            state=$(rotl 6 "$state")
            local score=$(head -1 <<<"$state")
            (( scores[player] += turn + score ))
            state=$(tail +2 <<< "$state")
        fi
            
    done
    tr ' ' '\n' <<< ${scores[*]} | sort -rn | head -1
}


tr -cd "[0-9 \n]" | while read players marbles; do
    marble_measurer $players $marbles
done
