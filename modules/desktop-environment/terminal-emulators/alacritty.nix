{ config, pkgs, themes, ... }:

{
  programs.alacritty = {
    enable = true;
    settings = {
      env = {
        TERM = "xterm-256color";
      };
      window = {
        padding = {
          x = 3;
          y = 3;
        };
      };
      font = {
        normal = {
          family = "Fira Code";
        };
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
      background_opacity = 0.85;
    };
  };

}
