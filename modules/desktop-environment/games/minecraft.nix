{ config, pkgs, lib, ... }:

with lib;

let
  cfg = config.custom.games.minecraft;
in
{
  options = {
    custom.games.minecraft = {
      enable = mkOption {
        default = false;
        description = ''
          Minecraft launcher
        '';
      };
    };
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      prismlauncher
    ];
  };
}
