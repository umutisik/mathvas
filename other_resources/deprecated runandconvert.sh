#!/bin/sh
# $0 is the script name, $1 id the first ARG, $2 is second...
NAME="$1"
NOPATH="$2"
runghc $NAME.hs $NAME
mogrify -format jpeg $NAME.bmp
rm $NAME.bmp

