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


echo "Copying .config"

cp $HOME/.config/i3/config home/.config/i3/config
cp $HOME/.config/dunst/dunstrc home/.config/dunst/dunstrc
cp $HOME/.config/i3blocks/config home/.config/i3blocks/config
cp $HOME/.config/termite/config home/.config/termite/config
cp $HOME/.config/compton.conf home/.config/compton.conf

echo "Copying Xresources"

cp $HOME/.Xresources home/.Xresources

echo "Copying bin"

cp $HOME/bin/apply-theme.sh home/bin/apply-theme.sh
cp $HOME/bin/lock.sh home/bin/lock.sh
cp $HOME/bin/blocks home/bin/blocks -r

echo "Copying bash, vim configs"

cp $HOME/.bashrc home/.bashrc
cp $HOME/.vimrc home/.vimrc
cp $HOME/.vim/colors home/.vim/ -r
