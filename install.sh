#!/bin/bash

echo "Installing deps"
sudo pacman -S -q --needed termite rofi compton dunst udiskie sbxkb > /dev/null 2>&1

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
cp .zshrc $HOME/

echo "Installing zsh"

[ -d $HOME/.oh-my-zsh ] && rm -Rf $HOME/.oh-my-zsh
git clone -q https://github.com/robbyrussell/oh-my-zsh.git $HOME/.oh-my-zsh
git clone -q https://github.com/zsh-users/zsh-autosuggestions.git $HOME/.oh-my-zsh/custom/plugins/zsh-autosuggestions
git clone -q  https://github.com/zsh-users/zsh-syntax-highlighting.git $HOME/.oh-my-zsh/custom/plugins/zsh-syntax-highlighting

[ ! -d $HOME/.vim ] && mkdir $HOME/.vim
[ ! -d $HOME/.vim/bundle ] && mkdir $HOME/.vim/bundle
[ ! -d $HOME/.vim/colors ] && mkdir $HOME/.vim/colors

cp .vim/colors $HOME/.vim/colors -r

[ -d $HOME/.vim/bundle/neobundle.vim ] && rm -Rf $HOME/.vim/bundle/neobundle.vim
git clone -q https://github.com/Shougo/neobundle.vim.git $HOME/.vim/bundle/neobundle.vim

echo "Installing bins"

[ ! -d $HOME/bin ] && mkdir $HOME/bin

cp bin $HOME/ -r

git clone -q https://github.com/iliayar/ColorsManager.git /tmp/colorMgr
gcc -lstdc++ /tmp/colorMgr/src/color-utils.cpp -o $HOME/bin/color-utils
rm -Rf /tmp/colorMgr

echo "Installing Themes"

[ -d $HOME/Themes ] && rm -Rf $HOME/Themes
git clone -q https://github.com/iliayar/MyThemes.git $HOME/Themes

$HOME/bin/apply-theme.sh Monokai
