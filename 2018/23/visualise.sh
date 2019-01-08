#!/bin/sh

set -e

! mkdir img
python visualiser.py < input
yes | ffmpeg -r 2 -i img/img%03d.png -c:v libvpx -crf 4  -b:v 1M out.webm
rm -r img
