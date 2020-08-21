#!/usr/bin/bash

cp ~/.cache/wal/colors.Xresources ~/.Xresources
xrdb -query > ~/.Xresources 
# Thing pywal not generate
# Xft.dpi:                        109
# *font: Hack 9
# Xcursor.theme: Adwaita
# URxvt*font:      xft:Hack:size=9
# URxvt*scrollBar: False
# URxvt*letterSpace: -1

cp ~/.cache/wal/termite.conf ~/.config/termite/config
cp ~/.cache/wal/Theme.hs ~/.xmonad/Theme.hs
cp ~/.cache/wal/Theme.hs ~/.config/xmobar/Theme.hs

emacsclient -e "(load-theme 'ewal-doom-one t nil)"
xdotool key super+Shift_L+c
