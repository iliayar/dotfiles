#!/bin/bash

PLAYER=spotifyd
PLAYER_CMD="~/.cargo/bin/spotifyd"

red="<fc=$(xrdb -query | grep color1: | gawk '{ print $2 }')>"
green="<fc=$(xrdb -query | grep color2: | gawk '{ print $2 }')>"
yellow="<fc=$(xrdb -query | grep color3: | gawk '{ print $2 }')>"
reset="</fc>"

state_f="<action=playerctl previous -p ${PLAYER}> 玲 </action><action=playerctl play-pause -p ${PLAYER}> %s </action><action=playerctl next -p ${PLAYER}> 怜 </action>"

state=$(timeout 2s playerctl status -p $PLAYER 2>&1 || echo "Timeout")

if [[ "$state" == "Timeout" ]]; then
    label=$(printf "${red}Timeout${reset}")
elif [[ "$state" == "Stopped" || "$state" == "Paused" || "$state" == "Playing" ]]; then
    track="$(timeout 2s playerctl metadata -f "{{artist}} - {{title}}" -p $PLAYER 2> /dev/null || echo "Timeout")"
    if [[ "$track" == "Timeout" ]]; then
        track="${red}Timeout${reset}"
        if [[ "$state" == "Paused" || "$state" == "Stopped" ]]; then
            icon=""
        elif [[ "$state" == "Playing" ]]; then
            icon="契"
        fi
        state="${red}$(printf "${state_f}" "${icon}")${reset}"
    elif [[ "$state" == "Paused" || "$state" == "Stopped" ]]; then
        state="${yellow}$(printf "${state_f}" "")${reset}"
    elif [[ "$state" == "Playing" ]]; then
        state="${green}$(printf "${state_f}" "契")${reset}"
    else
        state="${red}Unknown error${reset}"
    fi
    label=$(printf "<action=~/.xmonad/xmonadctl 13>%-.30s</action> %s" "${track}" "${state}")
else
    label="${red}${PLAYER} not found${reset} ${yellow}<action=${PLAYER_CMD}>RUN</action>${reset}"    
fi

echo "${label}"
