{ config, pkgs, lib, themes, ... }:

with lib;

let cfg = config.custom.de.terms;
in {

  options = {
    custom.de.terms = {
      alacritty = {
        enable = mkOption { default = false; };

        option_as_alt = mkOption { default = false; };
      };
    };
  };

  config = mkIf cfg.alacritty.enable {
    programs.alacritty = {
      enable = true;
      settings = {
        env = { TERM = "xterm-256color"; };

        window = {
          padding = {
            x = 3;
            y = 3;
          };
          opacity = 0.85;
        } // (if cfg.alacritty.option_as_alt then {
          option_as_alt = "Both";
        } else
          { });

        font = {
          normal = { family = "Fira Code"; };
          size = 9.0;
        };

        colors = {
          primary = {
            background = "${themes.background}";
            foreground = "${themes.foreground}";
          };
          cursor = {
            text = "${themes.cursorText}";
            cursor = "${themes.cursor}";
          };
          normal = {
            black = "${themes.black}";
            red = "${themes.red}";
            green = "${themes.green}";
            yellow = "${themes.yellow}";
            blue = "${themes.blue}";
            magenta = "${themes.magenta}";
            cyan = "${themes.cyan}";
            white = "${themes.white}";
          };
          bright = {
            black = "${themes.brightBlack}";
            red = "${themes.brightRed}";
            green = "${themes.brightGreen}";
            yellow = "${themes.brightYellow}";
            blue = "${themes.brightBlue}";
            magenta = "${themes.brightMagenta}";
            cyan = "${themes.brightCyan}";
            white = "${themes.brightWhite}";
          };
        };
      };
    };
  };

}
