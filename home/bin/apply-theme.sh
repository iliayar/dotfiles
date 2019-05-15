~/bin/color-utils -p ~/.Xresources -p $HOME/Themes/$1.x l mt ~/.config/termite/config mx ~/.Xresources mr ~/.Xresources
nitrogen --save --head=1 --set-zoom-fill $HOME/Themes/$1.png > /dev/null 2>&1
nitrogen --save --head=0 --set-zoom-fill $HOME/Themes/$1.png > /dev/null 2>&1
xrdb -merge $HOME/.Xresources
i3-msg restart
