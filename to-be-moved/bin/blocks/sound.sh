#!/bin/bash

if [[ "${BLOCK_BUTTON}" -eq 1 ]]; then
	pulsemixer --toggle-mute
fi

muted=$(pulsemixer --get-mute)
sound=$(pulsemixer --get-volume | awk '{print $1}')


if [[ muted -eq 1 ]]; then
	block=' '
else
	block="  $sound%"
fi

echo $block
