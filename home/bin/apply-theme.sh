#!/bin/bash
~/bin/color-utils -alpha $2 -p ~/.Xresources -p $HOME/Themes/$1.x l mt ~/.config/termite/config mx ~/.Xresources mr ~/.Xresources
nitrogen --set-zoom-fill $HOME/Themes/$1.png > /dev/null 2>&1
xrdb $HOME/.Xresources
[ -e $HOME/Themes/$1.sh ] && $HOME/Themes/$1.sh
i3-msg restart
