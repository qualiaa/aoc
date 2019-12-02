#!/usr/bin/awk -f
{ x=$1; while ((x=int(x/3) - 2) > 0) { acc += x }  } END { print acc }
