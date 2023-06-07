{ config, pkgs, lib, themes, ... }:

with lib;

let
  cfg = config.custom.de.wayland.river;
in
  {
    options = {
      custom.de.wayland.river = {
        enable = mkOption {
          default = false;
        };
      };
    };

    config = mkIf cfg.enable {
      home.packages = with pkgs; [
        river
        xwayland
      ];


      xdg.configFile."river/init" = {
        text = ''
        riverctl map normal Super Return spawn alacritty

        riverctl map normal Super+Shift E exit

        riverctl spawn "dbus-update-activation-environment --systemd WAYLAND_DISPLAY XDG_CURRENT_DESKTOP=river"
        riverctl spawn "systemctl --user import-environment WAYLAND_DISPLAY XDG_CURRENT_DESKTOP=river"
        '';
        executable = true;
      };
    };
  }
