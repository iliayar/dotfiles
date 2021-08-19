#!/bin/bash

PLAYER=spotify

uline_red="%{u$(xrdb -query | grep color1: | gawk '{ print $2 }')}"
uline_green="%{u$(xrdb -query | grep color2: | gawk '{ print $2 }')}"
uline_yellow="%{u$(xrdb -query | grep color3: | gawk '{ print $2 }')}"

if [[ "${BLOCK_BUTTON}" -eq 1 ]]; then
    playerctl -p $PLAYER play-pause 2> /dev/null
fi


if [[ $(playerctl status -p $PLAYER 2> /dev/null ) == "Paused" ]]; then
    state="${uline_yellow}▮▮"
else
    state="${uline_green}▶️"
fi

track="$(playerctl metadata -f "{{artist}} - {{title}}" -p $PLAYER 2> /dev/null)"

label="$state $track"

[[ -z $track ]] && label="${uline_red}$PLAYER not found"



echo "${label}"
