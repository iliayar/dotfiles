#!/bin/bash
DIR=$(dirname $(readlink -f $0))
ST_PWD=$PWD

cd $DIR

install_deps() {
    echo "Installing deps"

    sudo pacman -S  --needed zsh termite rofi compton dunst udiskie sbxkb nitrogen scrot pulsemixer imagemagick zathura clang gnu-free-fonts

    if [[ ! -e /bin/yay ]]; then 
    cd /tmp
    git clone https://aur.archlinux.org/yay.git
    cd yay
    makepkg -si
    cd $DIR
    fi

    cd $DIR
}

install_configs() {
    echo "Installing configs"
    
    cd home

    [ ! -d $HOME/.config ] && mkdir $HOME/.config

    cp .config/i3 $HOME/.config/ -r
    cp .config/dunst $HOME/.config/ -r
    cp .config/i3blocks $HOME/.config/ -r
    cp .config/termite $HOME/.config/ -r
    cp .config/compton.conf $HOME/.config
    cp .config/i3-scrot.conf $HOME/.config/i3-scrot.conf
    cp .config/mimeapps.list $HOME/.config/mimeapps.list

    cp .Xresources $HOME/
    cp .vimrc $HOME/
    cp .bashrc $HOME/
    cp .zshrc $HOME/

    cd $DIR
}

install_zsh() {
    echo "Installing zsh"

    [ -d $HOME/.oh-my-zsh ] && rm -Rf $HOME/.oh-my-zsh
    git clone -q https://github.com/robbyrussell/oh-my-zsh.git $HOME/.oh-my-zsh
    git clone -q https://github.com/zsh-users/zsh-autosuggestions.git $HOME/.oh-my-zsh/custom/plugins/zsh-autosuggestions
    git clone -q  https://github.com/zsh-users/zsh-syntax-highlighting.git $HOME/.oh-my-zsh/custom/plugins/zsh-syntax-highlighting
    chsh -s /bin/zsh

}

install_vim() {
    cd home

    echo "Installing vim"   

    [ ! -d $HOME/.vim ] && mkdir $HOME/.vim
    [ ! -d $HOME/.vim/bundle ] && mkdir $HOME/.vim/bundle
    [ ! -d $HOME/.vim/colors ] && mkdir $HOME/.vim/colors

    cp .vim/colors $HOME/.vim/ -r

    [ -d $HOME/.vim/bundle/neobundle.vim ] && rm -Rf $HOME/.vim/bundle/neobundle.vim
    git clone -q https://github.com/Shougo/neobundle.vim.git $HOME/.vim/bundle/neobundle.vim

    cd $DIR
}

install_bins() {
echo "Installing bins"
    cd home

    [ ! -d $HOME/bin ] && mkdir $HOME/bin

    cp bin $HOME/ -r

    git clone -q https://github.com/iliayar/ColorsManager.git /tmp/colorMgr
    gcc -lstdc++ /tmp/colorMgr/src/color-utils.cpp -o $HOME/bin/color-utils
    rm -Rf /tmp/colorMgr

    cd $DIR
}

install_themes() {
    echo "Installing Themes"

    [ -d $HOME/Themes ] && rm -Rf $HOME/Themes
    git clone -q https://github.com/iliayar/MyThemes.git $HOME/Themes
    
    $HOME/bin/apply-theme.sh Monokai 0.8

}

install_others() {
echo "Installing others"

    cd other

    yay -S ly-git i3-scrot

    sudo systemctl disable sddm
    sudo systemctl enable ly

    sudo cp xorg.conf /etc/X11/xorg.conf

    cd $DIR
}

echo "Select modules: "

printf "Dependecies(yay,pakages) [Y/n] " && read DEPS
printf "Configs(dotfiles) [Y/n] " && read CONFIGS
printf "Zsh [Y/n] " && read ZSH
printf "Vim [Y/n] " && read VIM
printf "Bins($HOME/bin) [Y/n] " && read BINS
printf "Themes [Y/n] " && read THEMES
printf "Others [Y/n] " && read OTHERS

[[ $DEPS != "n" ]] && install_deps
[[ $CONFIGS != "n" ]] && install_configs
[[ $ZSH != "n" ]] && install_zsh
[[ $VIM != "n" ]] && install_vim
[[ $BINS != "n" ]] && install_bins
[[ $THEMES != "n" ]] && install_themes
[[ $OTHERS != "n" ]] && install_others



cd $ST_PWD
