#!/bin/bash

DIR=$(dirname "$(readlink -f "$0")")
ST_PWD=$PWD

cd "$DIR" || exit

DEPS=()
AUR_DEPS=()

CONFIGS=()

OPERATIONS=()

MODULES=(
        common
        termite
        alacritty
        polybar
        zsh
        fish
        vim
        emacs
        doom_emacs
        i3
        xmonad
        plymouth
        bins
        themes
        others
    )

config() {
    config_path="$HOME/$1"
    target_dir=$(dirname "$config_path")
    if [[ -d "$config_path" ]]; then
       printf "$config_path exists, remove it? [Y/n]" && read -r rem
       [[ "$rem" != "n" ]] && rm -Rf "$config_path"
    fi

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
    [[ -n "${AUR_DEPS[*]}" ]] && yay -S "${AUR_DEPS[@]}"
    cd "$DIR" || exit
}

install_deps() {
    echo "Installing Deps"
    [[ -n "${DEPS[*]}" ]] && sudo pacman -S  --needed "${DEPS[@]}"
}

install_configs() {
    echo "Installing configs"

    cd home || exit

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
        neovim
    )
    CONFIGS+=(
        .config/nvim
    )
    OPERATIONS+=(
        install_vim
    )
}

install_vim() {
    sh -c 'curl -fLo "${XDG_DATA_HOME:-$HOME/.local/share}"/nvim/site/autoload/plug.vim --create-dirs \
       https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
}

## Emacs
emacs() {
    DEPS+=(
        emacs
    )
    CONFIGS+=(
        .emacs.d
    )
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
    sudo cp xorg.conf.d /etc/X11/ -r

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

for m in "${MODULES[@]}"; do
    printf "%s [Y/n] " "${m^}" && read -r "${m^^}"
done

for m in "${MODULES[@]}"; do
    state="${m^^}"
    [[ ${!state} != "n" ]] && $m
done

echo "DEPS: "
echo "${DEPS[@]}"

echo "AUR_DEPS: "
echo "${AUR_DEPS[@]}"

echo "CONFIGS: "
echo "${CONFIGS[@]}"

echo "OPERATIONS: "
echo "${OPERATIONS[@]}"

printf "Proceed installation [Y/n] " && read -r PROCEED
    
if [[ $PROCEED != "n" ]]; then
    install_deps
    install_aur_deps
    install_configs
    install_operations
fi

cd "$ST_PWD" || exit
