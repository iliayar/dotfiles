#!/bin/bash

KEYMAP=$1
COLS=4

FW=350
LH=15
X=0
W=1920

background="$(xrdb -query | grep ^*background: | gawk '{ print $2 }')"
foreground="$(xrdb -query | grep ^*foreground: | gawk '{ print $2 }')"
red="$(xrdb -query | grep *color1: | gawk '{ print $2 }')"
green="$(xrdb -query | grep *color2: | gawk '{ print $2 }')"
yellow="$(xrdb -query | grep *color3: | gawk '{ print $2 }')"

INFO=$(awk -v cols=$COLS -v label="${KEYMAP}" -v keycol="${yellow}" -v labelcol="${red}" -v fg="${foreground}" \
           '/END/ {exit}
            {
               split($0, keys, ": ");
               key_hint[i++] = sprintf ("^fg(%s)%15.15s ^fg(%s)%-30.30s", keycol, keys[1], fg, keys[2]);
            }
            END {
                printf ("^fg(%s)%s", labelcol, label)
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
(echo "$INFO"; cat) | dzen2 -l $(($N_LINES)) -fn "-*-Hack-*-*-*-*-14-*-*-*-*-*-*-*" -fg $foreground -bg $background -h $LH -x $X -y $Y -w $W -e onstart=uncollapse
