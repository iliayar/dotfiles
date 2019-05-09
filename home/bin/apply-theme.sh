~/bin/color-utils -p ~/.Xresources -p $HOME/Themes/$1.x l mt ~/.config/termite/config mx ~/.resources/.Xresources mr ~/.Xresources
nitrogen --head=1 --set-zoom-fill $HOME/Themes/$1.png > /dev/null 2>&1
nitrogen --head=0 --set-zoom-fill $HOME/Themes/$1.png > /dev/null 2>&1
xrdb -merge $HOME/.Xresources
i3-msg restart
