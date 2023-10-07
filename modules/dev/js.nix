{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.custom.dev.js;
in
{
  options = {
    custom.dev.js = {
      enable = mkOption {
        default = false;
      };
    };
  };


  config = mkIf cfg.enable {
    home.packages = with pkgs; [
        bun
        nodejs_20
    ];
  };
}
