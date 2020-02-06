#!/bin/bash
# echo "Creating dirs"
#
# mkdir home
# mkdir home/.config
# mkdir home/.config/{i3,dunst,i3blocks,termite}
# mkdir home/bin
# mkdir home/bin/blocks
# mkdir home/.vim
# mkdir home/.vim/colors


echo "Copying .config, .xsession"

cp $HOME/.xbindkeysrc home/.xbindkeysrc
cp $HOME/.vimrc home/.vimrc
cp $HOME/.config/i3/config home/.config/i3/config
cp $HOME/.config/dunst/dunstrc home/.config/dunst/dunstrc
cp $HOME/.config/i3blocks/config home/.config/i3blocks/config
cp $HOME/.config/termite/config home/.config/termite/config
cp $HOME/.config/compton.conf home/.config/compton.conf
cp $HOME/.config/i3-scrot.conf home/.config/i3-scrot.conf
cp $HOME/.config/mimeapps.list home/.config/mimeapps.list
cp $HOME/.config/rofi/config.rasi home/.config/rofi/config.rasi

echo "Copying Xresources"

cp $HOME/.Xresources home/.Xresources

echo "Copying bin"

cp $HOME/bin/apply-theme.sh home/bin/apply-theme.sh
cp $HOME/bin/lock.sh home/bin/lock.sh
cp $HOME/bin/blocks home/bin/ -r

echo "Copying zsh, bash, vim configs"

cp $HOME/.bashrc home/.bashrc

cp $HOME/.zshrc home/.zshrc

cp $HOME/.vim home/ -r

echo "Copying other"

cp /etc/X11/xorg.conf other/xorg.conf
