{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.custom.dev.embed;
in
{
  options = {
    custom.dev.embed = {
      enable = mkOption {
        default = false;
      };
    };
  };


  config = mkIf cfg.enable {
  home.packages = with pkgs; [
    platformio
  ];
  };
}
