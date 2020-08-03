#!/bin/bash

PLAYER=spotifyd

red="<fc=$(xrdb -query | grep color1: | gawk '{ print $2 }')>"
green="<fc=$(xrdb -query | grep color2: | gawk '{ print $2 }')>"
yellow="<fc=$(xrdb -query | grep color3: | gawk '{ print $2 }')>"
reset="</fc>"

if [[ "${BLOCK_BUTTON}" -eq 1 ]]; then
    playerctl -p $PLAYER play-pause 2> /dev/null
fi


state="<action=playerctl previous -p ${PLAYER}> 玲 </action><action=playerctl play-pause -p ${PLAYER}> %s </action><action=playerctl next -p ${PLAYER}> 怜 </action>"

if [[ $(playerctl status -p $PLAYER 2> /dev/null ) == "Paused" ]]; then
    state="${yellow}$(printf "${state}" "")${reset}"
else
    state="${green}$(printf "${state}" "契")${reset}"
fi

track="$(playerctl metadata -f "{{artist}} - {{title}}" -p $PLAYER 2> /dev/null)"

label=$(printf "<action=xdotool key super+m>%-.30s</action> %s" "${track}" "${state}")

[[ -z $track ]] && label="${red}$PLAYER not found${reset}"



echo "${label}"
