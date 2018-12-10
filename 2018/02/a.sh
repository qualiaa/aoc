#!/bin/sh

# three solutions, from worst to best
xargs -I{} sh -c "echo {} | grep -o . | sort | uniq -c | tr -d '\t[a-z]1[4-9]\n ';echo" < input | sed -E 's/2/a/;s///g;s/3/b/;s///g;s/(.)(.)/\1\n\2/' | sort | uniq -c | awk -F'\n' -v RS=":" '{print $1 * $2}'
xargs -I{} sh -c "echo {} | grep -o . | sort | uniq -c | tr -d '\t[a-z]1[4-9]' | sort -u; echo" < input | grep -v "^ *$" | sort | uniq -c | awk '{print $1}' | paste -sd'*' | bc
xargs -I{} sh -c "echo {} | grep -o . | sort | uniq -c | grep -o ' *[23]'| sort -u" < input | grep -v "^ *$"| sort | uniq -c | awk '{print $1}' | paste -sd'*' | bc
