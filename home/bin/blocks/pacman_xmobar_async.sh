#!/usr/bin/env bash

eval "export $(egrep -z DBUS_SESSION_BUS_ADDRESS /proc/$(pgrep -u $USER xmonad)/environ)"

SOCKET="/tmp/.updates_data"
INITIATOR="cron"

if [[ $1 == '1' ]]; then
   urxvt -e sudo pacman -Syyu
   INITIATOR="Manual update"
fi

if [[ $2 == '1' ]]; then
   urxvt -e yay -Syyu
   INITIATOR="Manual update"
fi

/usr/bin/notify-send "Updates" "Fetching pacman and aur updates" -a "${INITIATOR}" -i "/usr/share/icons/Adwaita/96x96/emblems/emblem-synchronizing-symbolic.symbolic.png"

[[ -p "$SOCKET" ]] || mkfifo $SOCKET

PACNUM="$(checkupdates | wc -l)"
AURNUM="$(yay -Qum | wc -l)"

red="<fc=$(xrdb -query | grep *color1: | gawk '{ print $2 }')>"
green="<fc=$(xrdb -query | grep *color2: | gawk '{ print $2 }')>"
yellow="<fc=$(xrdb -query | grep *color3: | gawk '{ print $2 }')>"
reset="</fc>"

PACMAN_COLOR=$green
AUR_COLOR=$green

if [ $PACNUM -ge 10 ]; then
    PACMAN_COLOR=$red
elif [ $PACNUM -ge 5 ]; then
    PACMAN_COLOR=$yellow
fi

if [ $AURNUM -ge 10 ]; then
    AUR_COLOR=$red
elif [ $AURNUM -ge 5 ]; then
    AUR_COLOR=$yellow
fi

SCRIPT="${HOME}/bin/blocks/pacman_xmobar_async.sh "

echo "<action=${SCRIPT} 1 0>${PACMAN_COLOR}Pacman: ${PACNUM}${reset}</action> <action=${SCRIPT} 0 1>${AUR_COLOR}AUR: ${AURNUM}${reset}</action>" >> $SOCKET
