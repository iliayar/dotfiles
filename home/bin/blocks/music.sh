#!/bin/bash

PLAYER=spotify

if [[ "${BLOCK_BUTTON}" -eq 1 ]]; then
    playerctl -p $PLAYER play-pause 2> /dev/null
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
