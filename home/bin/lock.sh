#!/usr/bin/env bash

tmpbg='/tmp/screen.png'

(( $# )) && { icon=$1; }

icon="$HOME/Themes/Neofetch.png"

[[ -e $tmpbg ]] && rm $tmpbg
scrot "$tmpbg"
convert "$tmpbg" -scale 10% -scale 1000% "$tmpbg"
convert "$tmpbg" "$icon" -gravity center -composite -matte "$tmpbg"
i3lock -i "$tmpbg"
# xset dpms force off
