#!/bin/bash

KEYMAP=$1
COLS=4

FW=350
LH=15
X=0
W=1920

INFO=$(awk -v cols=$COLS -v label="${KEYMAP}" \
           '/END/ {exit}
            {
               split($0, keys, ": ");
               key_hint[i++] = sprintf ("^fg(orange)%15.15s ^fg(white)%-30.30s", keys[1], keys[2]);
            }
            END {
                printf ("^fg(red)%s", label)
                print ""
                rows = int( ((i+1) / cols) +1)
                for (j=0; j<=i;) {
                    for (k=0; k < rows; k++) {
                         row[k] = row[k] key_hint[j++]
                    }
                }
                for (k=0; k < rows; k++) {print row[k]}
                print ""
            }')

echo "$INFO"

N_LINES=$(wc -l <<< "$INFO")
Y=$(($3 + $5 - ($LH * ($N_LINES+1)) - 19))
(echo "$INFO"; cat) | dzen2 -l $(($N_LINES)) -fn "-*-Hack Nerd Font Mono-*-*-*-*-14-*-*-*-*-*-*-*" -fg '#d5c4a1' -bg '#1d2021' -h $LH -x $X -y $Y -w $W -e onstart=uncollapse
