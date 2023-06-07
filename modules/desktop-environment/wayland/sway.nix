{ config, pkgs, lib, themes, ... }:

with lib;

let
  cfg = config.custom.de.wayland.sway;
in
{
  options = {
    custom.de.wayland.sway = {
      enable = mkOption {
        default = false;
      };
    };
  };

  config = mkIf cfg.enable {
    wayland.windowManager.sway = {
      enable = true;
      wrapperFeatures.gtk = true;
      config = {
        terminal = "alacritty";
        # output = {
        # };
      };
    };
  };
}
