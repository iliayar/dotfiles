#!/bin/bash

DIR=$(dirname "$(readlink -f "$0")")
ST_PWD=$PWD

cd "$DIR" || exit

DEPS=()
AUR_DEPS=()

CONFIGS=()

OPERATIONS=()

config() {
    target_dir=$(dirname "$HOME/$1")
    mkdir -p "$target_dir"
    ln -sf "$PWD/$1" "$target_dir"
}

install_aur_deps() {
    echo "Installing AUR"

    if [[ ! -e /bin/yay ]]; then
    cd /tmp || exit
    git clone https://aur.archlinux.org/yay.git
    cd yay || exit
    makepkg -si
    fi

    echo "Installing AUR Deps"
    [[ ! -z "${AUR_DEPS[@]}" ]] && yay -S "${AUR_DEPS[@]}"
    cd "$DIR" || exit
}

install_deps() {
    echo "Installing Deps"
    [[ ! -z "${DEPS[@]}" ]] && sudo pacman -S  --needed "${DEPS[@]}"
}

install_configs() {
    echo "Installing configs"

    cd home

    for cfg in "${CONFIGS[@]}"; do
        config "$cfg"
    done

    cd "$DIR" || exit 
}

install_operations() {
    for op in "${OPERATIONS[@]}"; do
        $op
    done
}

## Common utils
common() {
    DEPS+=(
        dunst
        udiskie
        sbxkb
        nitrogen
        scrot
        pulsemixer
        imagemagick
        zathura
        clang
        gnu-free-fonts
        fzf
        pcmanfm
        ttf-font
        python-pip
        shellcheck
        asciidoc
        libconfig
        base-devel
        light
        xbindkeys
    )
    AUR_DEPS+=(
        ttf-nerd-fonts-hack-complete-git
        ly-git
        gksu
    )
    OPERATIONS+=(
        install_picom
        config_user
    )
    CONFIGS+=(
        .xbindkeysrc
        .config/dunst
        .config/picom.conf
        .config/mimeapps.list
        .config/rofi/config.rasi
        .config/zathura
        .Xresources
        .bashrc
    )
}

install_picom() {
    cd /tmp || exit
    git clone https://github.com/tryone144/picom.git
    cd picom || exit
    git checkout feature/dual_kawase
    git submodule update --init --recursive
    meson --buildtype=release . build
    ninja -C build
    ninja -C build install
    cd "$DIR" || exit
}


config_user() {
    sudo usermod -a -G video "$USER"
}


## Polybar
polybar() {
    AUR_DEPS+=(
        polybar
    )
    CONFIGS+=(
        .config/polybar
    )
}

## Alacritty
alacritty() {
    DEPS+=(
        alacritty
    )
    CONFIGS+=(
        .config/alacritty/alacritty.yml
    )
}

## Termite
termite() {
    DEPS+=(
        termite
    )
    CONFIGS+=(
        .config/termite
    )
}

## Zsh
zsh() {
    DEPS+=(
        zsh
    )
    CONFIGS+=(
        .zshrc
    )
    OPERATIONS+=(
        install_zsh
    )
}

install_zsh() {
    echo "Installing zsh"
    [ -d "$HOME/.oh-my-zsh" ] && rm -Rf "$HOME/.oh-my-zsh"
    git clone -q https://github.com/robbyrussell/oh-my-zsh.git "$HOME/.oh-my-zsh"
    git clone -q https://github.com/zsh-users/zsh-autosuggestions.git "$HOME/.oh-my-zsh/custom/plugins/zsh-autosuggestions"
    git clone -q  https://github.com/zsh-users/zsh-syntax-highlighting.git "$HOME/.oh-my-zsh/custom/plugins/zsh-syntax-highlighting"
    cp home/.oh-my-zsh/themes/* "$HOME/.oh-my-zsh/themes/"
    chsh -s /bin/zsh
}


## Fish
fish() {
    DEPS+=(
        fish
    )
    OPERATIONS+=(
        install_fish
    )
}

install_fish() {
    echo "Installing fish"
    curl -L https://get.oh-my.fish | fish

    ./home/fish_conf.sh
    chsh -s /bin/fish
}

## Vim
vim() {
    DEPS+=(
        nvim
    )
    CONFIGS+=(
        .config/nvim
    )
    OPERATIONS+=(
        install_vim
    )
}

install_vim() {
    sh -c 'curl -fLo "${XDG_DATA_HOME:-$HOME/.local/share}"/nvim/site/autoload/plug.vim --create-dirs
       https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
}

## Doom Emacs
doom_emacs() {
    DEPS+=(
        emacs
    )
    CONFIGS+=(
        .doom.d
    )
    OPERATIONS+=(
        install_doom_emacs
    )
}

install_doom_emacs() {
    echo "Install Doom Emacs"
    [ -d "$HOME/.emacs.d" ] && rm -Rf "$HOME/.emacs.d"

    git clone https://github.com/hlissner/doom-emacs ~/.emacs.d
    ~/.emacs.d/bin/doom install
}

## Binaries
bins() {
    OPERATIONS+=(
        install_bins
    )
    CONFIGS+=(
        bin
    )
}
install_bins() {
echo "Installing bins"
    cd home || exit

    [ ! -d "$HOME/bin" ] && mkdir "$HOME/bin"

    git clone -q https://github.com/iliayar/ColorsManager.git /tmp/colorMgr
    cd /tmp/colorMgr || exit
    make
    make install
    rm -Rf /tmp/colorMgr

    cd "$DIR" || exit
}

## Themes
themes() {
    CONFIGS+=(
        Themes
    )
}

## i3
i3() {
    DEPS+=(
        i3
        i3blocks
    )
    AUR_DEPS+=(
        i3-scrot
    )
    CONFIGS+=(
        .config/i3blocks
        .config/i3
        .config/i3-scrot.conf
    )
    OPERATIONS+=(
        install_i3blocks
    )
}


install_i3blocks() {
    echo "Install i3blocks"
    cd /tmp || exit
    git clone https://github.com/vivien/i3blocks-contrib
    cd i3blocks-contrib || exit
    sudo mkdir /usr/lib/i3blocks
    BLOCK=iface       && sudo cp "$BLOCK/$BLOCK" "/usr/lib/i3blocks/$BLOCK"
    BLOCK=memory      && sudo cp "$BLOCK/$BLOCK" "/usr/lib/i3blocks/$BLOCK"
    BLOCK=disk        && sudo cp "$BLOCK/$BLOCK" "/usr/lib/i3blocks/$BLOCK"
    BLOCK=battery     && sudo cp "$BLOCK/$BLOCK" "/usr/lib/i3blocks/$BLOCK"
    BLOCK=temperature && sudo cp "$BLOCK/$BLOCK" "/usr/lib/i3blocks/$BLOCK"

    cd "$DIR" || exit
}

## Xmonad/xmobar

xmonad() {
    DEPS+=(
        xmonad-contrib
        xmobar
    )
    CONFIGS+=(
        .xmonad
        .config/xmobar
    )
}

## Others
others() {
    OPERATIONS+=(
        install_others
    )
}

install_others() {
    echo "Installing others"
    cd other || exit

    sudo systemctl disable sddm
    sudo systemctl enable ly
    
    sudo pacman -S nvidia
    sudo cp xorg.conf /etc/X11/xorg.conf

    cd "$DIR" || exit
}


## Plymouth
plymouth() {
    AUR_DEPS+=(
        plymouth
        plymouth-theme-arch-logo-new
    )
    OPERATIONS+=(
        plymouth_install
    )
}

plymouth_install() {
    sudo sed -i -e 's/base udev/base udev plymouth/' /etc/mkinitcpio.conf
    sudo sed -i -e 's/encrypt/plymouth-encrypt/' /etc/mkinitcpio.conf
    sudo sed -i -e 's/rw add_efi_memmap/rw add_efi_memmap quiet splash loglevel=3 rd.udev.log_priority=3 vt.global_cursor_default=0/' \
        /boot/EFI/refind/refind.conf
    sudo mkinitcpio -p linux
    sudo plymouth-set-default-theme -R arch-logo-new
}

echo "Select modules: "

printf "Common [Y/n] "      && read -r COMMON
printf "Termite [Y/n] "     && read -r TERMITE
printf "Alacritty [Y/n] "   && read -r ALACRITTY
printf "Polybar [Y/n] "     && read -r POLYBAR
printf "Zsh [Y/n] "         && read -r ZSH
printf "Fish [Y/n] "        && read -r FISH
printf "Vim [Y/n] "         && read -r VIM
printf "Doom Emacs  [Y/n] " && read -r DOOM_EMACS
printf "i3 [Y/n] "          && read -r I3
printf "Xmonad [Y/n] "      && read -r XMONAD
printf "Plymouth [Y/n] "    && read -r PLYMOUTH
printf "Bins [Y/n] "        && read -r BINS
printf "Themes [Y/n] "      && read -r THEMES
printf "Others [Y/n] "      && read -r OTHERS

[[ $COMMON != "n" ]] && common
[[ $TERMITE != "n" ]] && termite
[[ $ALACRITTY != "n" ]] && alacritty
[[ $POLYBAR != "n" ]] && polybar
[[ $ZSH != "n" ]] && zsh
[[ $FISH != "n" ]] && fish
[[ $VIM != "n" ]] && vim
[[ $DOOM_EMACS != "n" ]] && doom_emacs
[[ $I3 != "n" ]] && i3
[[ $XMONAD != "n" ]] && xmonad
[[ $PLYMOUTH != "n" ]] && plymouth
[[ $BINS != "n" ]] && bins
[[ $THEMES != "n" ]] && themes
[[ $OTHERS != "n" ]] && others

echo "DEPS: "
echo "${DEPS[@]}"

echo "AUR_DEPS: "
echo "${AUR_DEPS[@]}"

echo "CONFIGS: "
echo "${CONFIGS[@]}"

echo "OPERATIONS: "
echo "${OPERATIONS[@]}"

install_deps
install_aur_deps
install_configs
install_operations


cd "$ST_PWD" || exit
