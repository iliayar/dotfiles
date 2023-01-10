{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.custom.dev.reverse;
in
{
  options = {
    custom.dev.reverse = {
      enable = mkOption {
        default = false;
      };
    };
  };


  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      cutter
    ];
  };
}
