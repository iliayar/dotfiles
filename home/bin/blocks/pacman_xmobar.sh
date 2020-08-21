notify-send "Updates" "Fetching pacman and aur updates" -a "XMobar" -i "/usr/share/icons/Adwaita/96x96/emblems/emblem-synchronizing-symbolic.symbolic.png"

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

echo -e "<action=urxvt -e sudo pacman -Syyu>${PACMAN_COLOR}Pacman: ${PACNUM}${reset}</action> <action=urxvt -e yay -Syyu>${AUR_COLOR}AUR: ${AURNUM}${reset}</action>"
