{ config, pkgs, lib, themes, ... }:

with lib;

let
  cfg = config.custom.de.pointer;
in
{
  options = {
    custom.de.pointer = {
      enable = mkOption {
        default = false;
      };
    };
  };

  config = mkIf cfg.enable {
    home.pointerCursor = {
      package = pkgs.vanilla-dmz;
      name = "Vanilla-DMZ";
      size = 16;
      x11.enable = true;
    };
  };
}
