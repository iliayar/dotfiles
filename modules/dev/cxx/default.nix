{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.custom.dev.cpp;
in
{
  options = {
    custom.dev.cpp = {
      enable = mkOption {
        default = false;
      };
    };
  };


  config = mkIf cfg.enable {
    home.packages = with pkgs; [
    ];
  };
}
