#!/bin/bash

cd home

echo "Installing .config"

[ ! -d $HOME/.config ] && mkdir $HOME/.config

cp .config/i3 $HOME/.config/ -r
cp .config/dunst $HOME/.config/ -r
cp .config/i3blocks $HOME/.config/ -r
cp .config/termite $HOME/.config/ -r
cp .config/compton.conf $HOME/.config

echo "Installing bash, vim configs"

cp .Xresources $HOME/
cp .vimrc $HOME/
cp .bashrc $HOME/

[ ! -d $HOME/.vim ] && mkdir $HOME/.vim
[ ! -d $HOME/.vim/bundle ] && mkdir $HOME/.vim/bundle
[ ! -d $HOME/.vim/colors ] && mkdir $HOME/.vim/colors

cp .vim/colors $HOME/.vim/colors -r

[ -d $HOME/.vim/bundle/neobundle.vim ] && rm -Rf $HOME/.vim/bundle/neobundle.vim
git clone https://github.com/Shougo/neobundle.vim.git $HOME/.vim/bundle/neobundle.vim

echo "Installing bins"

[ ! -d $HOME/bin ] && mkdir $HOME/bin

cp bin $HOME/bin -r

git clone https://github.com/iliayar/ColorsManager.git /tmp/colorMgr
gcc -lstdc++ /tmp/colorMgr/src/color-utils.cpp -o $HOME/bin/color-utils
rm -Rf /tmp/colorMgr

echo "Installing Themes"

[ -d $HOME/Themes ] && rm -Rf $HOME/Themes
git clone https://github.com/iliayar/MyThemes.git $HOME/Themes

