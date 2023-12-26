{ config, pkgs, lib, themes, ... }:

with lib;

let cfg = config.custom.de.terms;
in {

  options = {
    custom.de.terms = {
      wezterm = {
        enable = mkOption { default = false; };
        package = mkOption {
            default = pkgs.wezterm-fixed;
        };
      };
    };
  };

  config = mkIf cfg.wezterm.enable {
    programs.wezterm = {
      package = cfg.wezterm.package;
      enable = true;

      colorSchemes = {
        my-theme = {
          ansi = [
            "${themes.color0}"
            "${themes.color1}"
            "${themes.color2}"
            "${themes.color3}"
            "${themes.color4}"
            "${themes.color5}"
            "${themes.color6}"
            "${themes.color7}"
          ];

          brights = [
            "${themes.color8}"
            "${themes.color9}"
            "${themes.color10}"
            "${themes.color11}"
            "${themes.color12}"
            "${themes.color13}"
            "${themes.color14}"
            "${themes.color15}"
          ];

          background = "${themes.background}";
          foreground = "${themes.foreground}";

          selection_bg = "${themes.foreground}";
          selection_fg = "${themes.background}";

          cursor_bg = "${themes.cursor}";
          cursor_fg = "${themes.cursorText}";
          cursor_border = "${themes.cursor}";
        };
      };

      extraConfig = builtins.readFile ./wezterm.lua;
    };
  };

}
