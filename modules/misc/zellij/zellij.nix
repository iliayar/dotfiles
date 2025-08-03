{ config, pkgs, lib, themes, ... }:

with lib;

let
  cfg = config.custom.misc;
in
{
  options = {
    custom.misc.zellij = {
      enable = mkOption {
        default = false;
      };
    };
  };

  config = mkMerge [
    (mkIf (cfg.enable && cfg.zellij.enable) {
      programs.zellij = {
        enable = true;
      };

      xdg.configFile."zellij/config.kdl".source = ./zellij.kdl;
      xdg.configFile."zellij/layouts".source = ./layouts;
    })
  ];
}
