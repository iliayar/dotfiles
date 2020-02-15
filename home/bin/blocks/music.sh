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

track="$(playerctl metadata -f "{{artist}} - {{title}}" -p $PLAYER)"

mpd="$state $track"

[[ -z $track ]] && mpd="$PLAYER not found"



echo "${mpd}"
