#!/bin/bash

PLAYER=spotify

if [[ "${BLOCK_BUTTON}" -eq 1 ]]; then
    playerctl play-pause -p $PLAYER
fi


if [[ $(playerctl status -p $PLAYER) == "Paused" ]]; then
    state="▮▮"
else
    state="▶️"
fi

mpd="$state $(playerctl metadata -f "{{artist}} - {{title}}" -p $PLAYER)"

echo "${mpd}"
