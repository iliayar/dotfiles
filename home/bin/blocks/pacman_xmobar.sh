notify-send "Fetching pacman and aur updates"

PACNUM="$(checkupdates | wc -l)"
AURNUM="$(yay -Qum | wc -l)"

red="<fc=$(xrdb -query | grep color1: | gawk '{ print $2 }')>"
green="<fc=$(xrdb -query | grep color10: | gawk '{ print $2 }')>"
yellow="<fc=$(xrdb -query | grep color9: | gawk '{ print $2 }')>"
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

echo -e "${PACMAN_COLOR}Pacman: ${PACNUM}${reset} ${AUR_COLOR}AUR: ${AURNUM}${reset}"
