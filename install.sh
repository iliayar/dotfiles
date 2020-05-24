#!/bin/bash
DIR=$(dirname $(readlink -f $0))
ST_PWD=$PWD

cd $DIR


config() {
    ln -sf $PWD/$1 $(echo $HOME/$1 | sed "s/[^\/]*$//")
}


install_deps() {
    echo "Installing deps"

    sudo pacman -S  --needed zsh termite rofi  dunst udiskie sbxkb nitrogen scrot pulsemixer imagemagick zathura clang gnu-free-fonts pcmanfm  ttf-font python-pip transset-df shellcheck asciidoc libconfig base-devel fish alacritty light xbindkeys


    if [[ ! -e /bin/yay ]]; then 
    cd /tmp
    git clone https://aur.archlinux.org/yay.git
    cd yay
    makepkg -si
    cd ../
    git clone https://github.com/tryone144/picom.git
    cd picom
    git checkout feature/dual_kawase
    git submodule update --init --recursive
    meson --buildtype=release . build
    ninja -C build
    ninja -C build install
    cd $DIR
    fi

    yay -S gksu nerd-fonts-hack polybar ly-git i3-scrot

    cd $DIR
}

install_configs() {
    echo "Installing configs"
    
    cd home

    [ ! -d $HOME/.config ] && mkdir $HOME/.config

    [ ! -d $HOME/.config/rofi ] && mkdir $HOME/.config/rofi
    [ ! -d $HOME/.config/polybar ] && mkdir $HOME/.config/polybar
    [ ! -d $HOME/.config/alacritty ] && mkdir $HOME/.config/alacritty

    sudo usermod -a -G video $USER

    config .xbindkeysrc
    config .config/i3
    config .config/dunst
    config .config/i3blocks
    config .config/termite
    config .config/picom.conf
    config .config/i3-scrot.conf
    config .config/mimeapps.list
    config .config/rofi/config.rasi
    config .config/polybar/config
    config .config/polybar/launch.sh
    config .config/alacritty/alacritty.yml

    config .Xresources
    config .bashrc
    config .zshrc

    cd $DIR
}

install_shell() {
    echo "Installing zsh"

    [ -d $HOME/.oh-my-zsh ] && rm -Rf $HOME/.oh-my-zsh
    git clone -q https://github.com/robbyrussell/oh-my-zsh.git $HOME/.oh-my-zsh
    git clone -q https://github.com/zsh-users/zsh-autosuggestions.git $HOME/.oh-my-zsh/custom/plugins/zsh-autosuggestions
    git clone -q  https://github.com/zsh-users/zsh-syntax-highlighting.git $HOME/.oh-my-zsh/custom/plugins/zsh-syntax-highlighting
    cp home/.oh-my-zsh/themes/* $HOME/.oh-my-zsh/themes/
    chsh -s /bin/zsh

    echo "Installing fish"
    curl -L https://get.oh-my.fish | fish

    ./home/fish_conf.sh

}

install_vim() {
    cd home

    echo "Installing vim"   
   

    curl -sLf https://spacevim.org/install.sh | bash

    config .myvim

    cd $DIR
}


install_spacemacs() {

    cd home


    echo "Cleaning .emacs.d"
    [ -d "$HOME/.emacs.d" ] && rm -Rf "$HOME/.emacs.d"

    sudo pacman -S texlive-most texlive-langcyrillic jdk11-openjdk

    echo "Installing spacemacs"
    git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d

    config .spacemacs

    cd $DIR
}

install_doom_emacs() {
   

    cd home

    sudo pacman -S emacs 
    echo "Cleaning .emacs.d"
    [ -d "$HOME/.emacs.d" ] && rm -Rf "$HOME/.emacs.d"

    git clone https://github.com/hlissner/doom-emacs ~/.emacs.d
    ~/.emacs.d/bin/doom install


    echo "Cleaning .doom.d"
    [ -d "$HOME/.doom.d" ] && rm -Rf "$HOME/.doom.d"

    config .doom.d

    ~/.emacs.d/bin/doom sync

    cd $DIR
}

install_bins() {
echo "Installing bins"
    cd home

    [ ! -d $HOME/bin ] && mkdir $HOME/bin

    cp bin $HOME/ -r

    git clone -q https://github.com/iliayar/ColorsManager.git /tmp/colorMgr
    cd /tmp/colorMgr
    make
    make install
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

    sudo systemctl disable sddm
    sudo systemctl enable ly
    
    sudo pacman -S nvidia
    sudo cp xorg.conf /etc/X11/xorg.conf


    cd /tmp
    git clone https://github.com/vivien/i3blocks-contrib
    cd i3blocks-contrib
    sudo mkdir /usr/lib/i3blocks
    BLOCK=iface && sudo cp $BLOCK/$BLOCK /usr/lib/i3blocks/$BLOCK
    BLOCK=memory && sudo cp $BLOCK/$BLOCK /usr/lib/i3blocks/$BLOCK
    BLOCK=disk && sudo cp $BLOCK/$BLOCK /usr/lib/i3blocks/$BLOCK
    BLOCK=battery && sudo cp $BLOCK/$BLOCK /usr/lib/i3blocks/$BLOCK
    BLOCK=temperature && sudo cp $BLOCK/$BLOCK /usr/lib/i3blocks/$BLOCK

    cd $DIR
}

echo "Select modules: "

printf "Dependecies(yay,pakages) [Y/n] " && read DEPS
printf "Configs(dotfiles) [Y/n] " && read CONFIGS
printf "Shell [Y/n] " && read _SHELL
printf "Vim [Y/n] " && read VIM
printf "Spacemacs [Y/n] " && read SPACEMACS
printf "Doom Emacs  [Y/n] " && read DOOM_EMACS
printf "Bins($HOME/bin) [Y/n] " && read BINS
printf "Themes [Y/n] " && read THEMES
printf "Others [Y/n] " && read OTHERS

[[ $DEPS != "n" ]] && install_deps
[[ $CONFIGS != "n" ]] && install_configs
[[ $_SHELL != "n" ]] && install_shell
[[ $VIM != "n" ]] && install_vim
[[ $BINS != "n" ]] && install_bins
[[ $THEMES != "n" ]] && install_themes
[[ $OTHERS != "n" ]] && install_others
[[ $SPACEMACS != "n" ]] && install_spacemacs
[[ $DOOM_EMACS != "n" ]] && install_doom_emacs



cd $ST_PWD
