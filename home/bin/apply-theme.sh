~/bin/color-utils -alpha $2 -p ~/.Xresources -p $HOME/Themes/$1.x l mt ~/.config/termite/config mx ~/.Xresources mr ~/.Xresources
sudo cp $HOME/Themes/$1.png /usr/share/sddm/themes/sddm-sugar-candy/Backgrounds/Mountain.jpg
nitrogen --head=1 --set-zoom-fill $HOME/Themes/$1.png > /dev/null 2>&1
nitrogen --head=0 --set-zoom-fill $HOME/Themes/$1.png > /dev/null 2>&1
xrdb $HOME/.Xresources
i3-msg restart
[ -e $HOME/Themes/$1.sh ] && $HOME/Themes/$1.sh
