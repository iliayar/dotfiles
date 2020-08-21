#!/usr/bin/bash

cp ~/.cache/wal/colors.Xresources ~/.Xresources
cat >> ~/.Xresources <<EOF
Xft.dpi:                        109
*font: Hack 9
Xcursor.theme: Adwaita
EOF
xrdb -merge ~/.Xresources
cp ~/.cache/wal/termite.conf ~/.config/termite/config
cp ~/.cache/wal/Theme.hs ~/.xmonad/Theme.hs
cp ~/.cache/wal/Theme.hs ~/.config/xmobar/Theme.hs
