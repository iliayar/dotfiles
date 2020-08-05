#!/bin/bash

PLAYER=spotifyd
PLAYER_CMD="~/.cargo/bin/spotifyd"

red="<fc=$(xrdb -query | grep color1: | gawk '{ print $2 }')>"
green="<fc=$(xrdb -query | grep color2: | gawk '{ print $2 }')>"
yellow="<fc=$(xrdb -query | grep color3: | gawk '{ print $2 }')>"
reset="</fc>"

SOCKET="/tmp/.music_data"

[[ -p "$SOCKET" ]] || mkfifo $SOCKET

state_f="<action=playerctl previous -p ${PLAYER}> 玲 </action><action=playerctl play-pause -p ${PLAYER}> %s </action><action=playerctl next -p ${PLAYER}> 怜 </action>"

while true; do
    state=$(playerctl status -p $PLAYER 2> /dev/null)

    if [[ -z "$state" ]]; then
        label="${red}${PLAYER} not found${reset} ${yellow}<action=${PLAYER_CMD}>RUN</action>${reset}"    
    else
        track="$(playerctl metadata -f "{{artist}} - {{title}}" -p $PLAYER 2> /dev/null)"
        if [[ "$state" == "Paused" || "$state" == "Stopped" ]]; then
            state="${yellow}$(printf "${state_f}" "")${reset}"
        elif [[ "$state" == "Playing" ]]; then
            state="${green}$(printf "${state_f}" "契")${reset}"
        else
            state="${red}Unknown error${reset}"
        fi
        label=$(printf "<action=~/.xmonad/xmonadctl 13>%-.30s</action> %s" "${track}" "${state}")
    fi
    echo "${label}" >> $SOCKET
    # echo "${label}"
    sleep 1
done
