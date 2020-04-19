#!/bin/bash

notify-send "Fetching pacman and aur updates"

PACNUM="$(checkupdates | wc -l)"
AURNUM="$(yay -Qum | wc -l)"

red="%{F$(xrdb -query | grep color1: | gawk '{ print $2 }')}"
green="%{F$(xrdb -query | grep color2: | gawk '{ print $2 }')}"
yellow="%{F$(xrdb -query | grep color3: | gawk '{ print $2 }')}"
reset="%{F-}"
uline_red="%{u$(xrdb -query | grep color1: | gawk '{ print $2 }')}"
uline_green="%{u$(xrdb -query | grep color2: | gawk '{ print $2 }')}"
uline_yellow="%{u$(xrdb -query | grep color3: | gawk '{ print $2 }')}"

PACMAN_COLOR=$reset
AUR_COLOR=$reset
ULINE=$uline_green

if [ $PACNUM -ge 10 ]; then
    PACMAN_COLOR=$red
    ULINE=$uline_red
elif [ $PACNUM -ge 5 ]; then
    PACMAN_COLOR=$yellow
    ULINE=$uline_yellow
fi

if [ $AURNUM -ge 10 ]; then
    AUR_COLOR=$red
    ULINE=$uline_red
elif [ $AURNUM -ge 5 ]; then
    AUR_COLOR=$yellow
    ULINE=$uline_yellow
fi

echo -e "${ULINE}${PACMAN_COLOR}Pacman: ${PACNUM}${reset} ${AUR_COLOR}AUR: ${AURNUM}${reset}"
