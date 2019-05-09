~/bin/color-utils -p ~/.resources/.Xresources -p $HOME/Themes/$1.x l mt ~/.config/termite/config mx ~/.resources/.Xresources mr ~/.resources/.Xresources
nitrogen --head=1 --set-zoom-fill $HOME/Themes/$1.png
nitrogen --head=0 --set-zoom-fill $HOME/Themes/$1.png
xrdb -merge $HOME/.resources/.Xresources
i3-msg restart
