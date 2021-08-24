{ pkgs, wallpapers, ... }:

let
  locker = pkgs.writeShellScriptBin "locker" ''
    #!${pkgs.bash}/bin/bash
    
    tmpbg='/tmp/screen.png'
    
    (( $# )) && { icon=$1; }
    
    icon="${wallpapers}/Neofetch.png"
    
    [[ -e $tmpbg ]] && rm $tmpbg
    ${pkgs.scrot}/bin/scrot "$tmpbg"
    ${pkgs.imagemagick}/bin/convert "$tmpbg" -scale 10% -scale 1000% "$tmpbg"
    ${pkgs.imagemagick}/bin/convert "$tmpbg" "$icon" -gravity center -composite -matte "$tmpbg"
    ${pkgs.i3lock}/bin/i3lock -i "$tmpbg"
  '';
in
{
  services.screen-locker = {
    enable = true;
    lockCmd = "${locker}/bin/locker";
    inactiveInterval = 5;
  };

}
