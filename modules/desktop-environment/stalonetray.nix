{ config, pkgs, lib, themes, ... }:

with lib;

let
  cfg = config.custom.de.tray;
in
{

  options = {
    custom.de.tray = {
      enable = mkOption {
        default = false;
      };
    };
  };

  config = mkIf cfg.enable {
    services.stalonetray = {
      enable = true;
      config = {
        geometry = "5x1+20+20";
        icon_size = 20;
        transparent = true;
        sticky = false;
        window_type = "dock";
        window_strut = "auto";
        skip_taskbar = 1;
        icon_gravity = "NW";
        background = "${themes.background}";
      };
    };
  };
}
