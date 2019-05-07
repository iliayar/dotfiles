#!/bin/bash

if [[ "${BLOCK_BUTTON}" -eq 1 ]]; then
	mpc toggle
fi


if [[ $(mpc | awk '/(paused|playing)/ {print $1}') == "[paused]" ]]; then
	state="▮▮"
else
	state="▶️"
fi

progress=$(mpc | awk '/(paused|playing)/ {print $3}')

mpd="$state $(mpc -f "%artist% - %title%" current) ($progress)"

echo "${mpd}"
