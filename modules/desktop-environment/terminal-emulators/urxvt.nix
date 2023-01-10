{ config, pkgs, lib, themes, ... }:

with lib;

let
  cfg = config.custom.de.terms;
in
{

  options = {
    custom.de.terms = {
      urxvt = {
        enable = mkOption {
          default = false;
        };
      };
    };
  };

  config = mkIf cfg.urxvt.enable {
    programs.urxvt = {
      enable = true;
      fonts = [ "xft:${themes.font}:size=11" ];
      scroll.bar.enable = false;
      extraConfig = {
        color0 = "${themes.color0}";
        color1 = "${themes.color1}";
        color10 = "${themes.color10}";
        color11 = "${themes.color11}";
        color12 = "${themes.color12}";
        color13 = "${themes.color13}";
        color14 = "${themes.color14}";
        color15 = "${themes.color15}";
        color2 = "${themes.color2}";
        color3 = "${themes.color3}";
        color4 = "${themes.color4}";
        color5 = "${themes.color5}";
        color6 = "${themes.color6}";
        color7 = "${themes.color7}";
        color8 = "${themes.color8}";
        color9 = "${themes.color9}";
        background = "[80]${themes.background}";
        cursorColor = "${themes.cursor}";
        depth = 32;
        foreground = "${themes.foreground}";
      };
    };
  };
}
