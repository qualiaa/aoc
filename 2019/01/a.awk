#!/usr/bin/awk -f
{ x += int($1/3) } END { print x - 2*NR }
