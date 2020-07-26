#!/bin/bash

PLAYER=spotify

red="<fc=$(xrdb -query | grep color1: | gawk '{ print $2 }')>"
green="<fc=$(xrdb -query | grep color2: | gawk '{ print $2 }')>"
yellow="<fc=$(xrdb -query | grep color3: | gawk '{ print $2 }')>"
reset="</fc>"

if [[ "${BLOCK_BUTTON}" -eq 1 ]]; then
    playerctl -p $PLAYER play-pause 2> /dev/null
fi


if [[ $(playerctl status -p $PLAYER 2> /dev/null ) == "Paused" ]]; then
    state="${yellow}▮▮${reset}"
else
    state="${green}▶${reset}"
fi

track="$(playerctl metadata -f "{{artist}} - {{title}}" -p $PLAYER 2> /dev/null)"

label="$state $track"

[[ -z $track ]] && label="${red}$PLAYER not found${reset}"



echo "${label}"
