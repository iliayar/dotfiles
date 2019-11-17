#!/bin/bash
DIR=$(dirname $(readlink -f $0))
ST_PWD=$PWD

cd $DIR

install_deps() {
    echo "Installing deps"

# termite sbxkb i3-gaps neovim 


    sudo apt-get install -y autoconf
    sudo apt-get install -y automake
    sudo apt-get install -y build-essential
    sudo apt-get install -y libtool
    sudo apt-get install -y xutils-dev xcb libxcb-composite0-dev
    sudo apt-get install -y doxygen

    sudo apt install -y zsh rofi dunst nitrogen pulsemixer imagemagick \
    scrot zathura clang python3 python3-pip python python-pip \
    asciidoc libconfig++-dev libconfig9

    apt install -y g++ libgtk-3-dev gtk-doc-tools gnutls-bin \
    valac intltool libpcre2-dev libglib3.0-cil-dev libgnutls28-dev \
    libgirepository1.0-dev libxml2-utils gperf build-essential \
    gir1.2-vte-2.91 cmake



    rm -rf /tmp/Airblader

    git clone https://github.com/Airblader/i3.git /tmp/Airblader
    cd /tmp/Airblader
    autoreconf --force --install
    rm -rf build/
    mkdir -p build && cd build/
    ../configure --prefix=/usr --sysconfdir=/etc --disable-sanitizers

    make && sudo make install

    rm -rf /tmp/Airblader

    rm -Rf /tmp/compton
    git clone https://github.com/tryone144/compton.git /tmp/compton
    cd /tmp/compton
    make
    sudo make install

    cd $DIR
}

install_configs() {
    echo "Installing configs"
    
    cd home

    [ ! -d $HOME/.config ] && mkdir $HOME/.config

    [ ! -d $HOME/.config/nvim ] && mkdir $HOME/.config/nvim
    [ ! -d $HOME/.config/rofi ] && mkdir $HOME/.config/rofi

    cp .config/i3 $HOME/.config/ -r
    cp .config/dunst $HOME/.config/ -r
    cp .config/i3blocks $HOME/.config/ -r
    cp .config/termite $HOME/.config/ -r
    cp .config/compton.conf $HOME/.config
    cp .config/i3-scrot.conf $HOME/.config/i3-scrot.conf
    cp .config/mimeapps.list $HOME/.config/mimeapps.list
    cp .config/nvim/init.vim $HOME/.config/nvim/init.vim
    cp .config/rofi/config.rasi $HOME/.config/rofi/config.rasi

    cp .Xresources $HOME/
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
    
    curl -fLo ~/.local/share/nvim/site/autoload/plug.vim --create-dirs \
    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

    sudo python3 -m pip install pynvim jedi yapf pylint
    sudo python -m pip install pynvim jedi yapf pylint


    [ ! -d $HOME/.config/nvim/colors ] && mkdir $HOME/.config/nvim/colors
    cp .config/nvim/colors/monokai.vim $HOME/.config/nvim/colors/monokai.vim

    cd $DIR
}

install_bins() {
echo "Installing bins"
    cd home

    [ ! -d $HOME/bin ] && mkdir $HOME/bin

    cp bin $HOME/ -r

    git clone https://github.com/iliayar/ColorsManager.git /tmp/colorMgr
    gcc -lstdc++ /tmp/colorMgr/src/color-utils.cpp -o $HOME/bin/color-utils
    rm -Rf /tmp/colorMgr

    cd $DIR
}

install_themes() {
    echo "Installing Themes"

    [ -d $HOME/Themes ] && rm -Rf $HOME/Themes
    git clone https://github.com/iliayar/MyThemes.git $HOME/Themes
    
    $HOME/bin/apply-theme.sh Monokai 0.8

}

install_others() {
echo "Installing others"

    cd other

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
