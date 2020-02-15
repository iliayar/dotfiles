#!/bin/bash

if [[ "${BLOCK_BUTTON}" -eq 1 ]]; then
    playerctl play-pause
fi


if [[ $(playerctl status) == "Paused" ]]; then
    state="▮▮"
else
    state="▶️"
fi

mpd="$state $(playerctl metadata -f "{{artist}} - {{title}}")"

echo "${mpd}"
