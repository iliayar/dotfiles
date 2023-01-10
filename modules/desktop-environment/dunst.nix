{ config, pkgs, lib, themes, ... }:

with lib;

let
  cfg = config.custom.de.dunst;
in
{
  options = {
    custom.de.dunst = {
      enable = mkOption {
        default = false;
      };
    };
  };

  config = mkIf cfg.enable {
    home.packages = [
      pkgs.libnotify
    ];
    services.dunst = {
      enable = true;
      settings = {
        global = {
          font = "${themes.font}";
          allow_markup = true;
          format = ''<b>%s</b> %p\n%b\n<span size="7500">%a</span>'';
          max_icon_size = 80;
          corner_radius=10;
          sort = "yes";
          indicate_hidden = "yes";
          alignment = "left";
          bounce_freq = 5;
          show_age_threshold = 60;
          word_wrap = "yes";
          ignore_newline = "no";
          geometry = "400x5-15-30";
          shrink = "yes";
          transparency = 40;
          idle_threshold = 1;
          monitor = 0;
          follow = "none";
          sticky_history = "yes";
          history_length = 20;
          show_indicators = "yes";
          line_height = 0;
          separator_height = 1;
          padding = 8;
          horizontal_padding = 10;
          separator_color = "${themes.blue}";
          startup_notification = false;
          dmenu = "${pkgs.dmenu}/bin/dmenu -p dunst";
          browser = "brave";
          icon_position = "left";
        };

        frame = {
          width = 1;
          color = "${themes.blue}";
        };

        urgency_low = {
          background = "${themes.background}";
          foreground = "${themes.foreground}";
          timeout = 10;
        };

        urgency_normal = {
          background = "${themes.background}";
          foreground = "${themes.foreground}";
          timeout = 10;
        };

        urgency_critical = {
          background = "${themes.background}";
          foreground = "${themes.red}";
          timeout = 0;
        };
      };
    };
  };
}
