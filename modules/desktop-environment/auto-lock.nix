{ config, pkgs, lib, wallpapers, ... }:

with lib;

let
  cfg = config.custom.de.lock;

  locker = pkgs.writeShellScriptBin "locker" ''
    #!${pkgs.bash}/bin/bash
    
    tmpbg='/tmp/screen.png'
    
    (( $# )) && { icon=$1; }
    
    icon="${wallpapers}/Neofetch.png"
    
    ${pkgs.scrot}/bin/scrot -o "$tmpbg"
    ${pkgs.imagemagick}/bin/convert "$tmpbg" -scale 10% -scale 1000% "$tmpbg"
    ${pkgs.imagemagick}/bin/convert "$tmpbg" "$icon" -gravity center -composite -matte "$tmpbg"
    ${pkgs.i3lock}/bin/i3lock -i "$tmpbg"
  '';
in
{
  options = {
    custom.de.lock = {
      enable = mkOption {
        default = false;
      };
    };
  };

  config = mkIf cfg.enable {
    services.screen-locker = {
      enable = true;
      lockCmd = "${locker}/bin/locker";
      inactiveInterval = 5;
    };

    home.packages = [ locker ];
  };
}
