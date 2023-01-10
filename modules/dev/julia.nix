{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.custom.dev.julia;
in
{
  options = {
    custom.dev.julia = {
      enable = mkOption {
        default = false;
      };
    };
  };


  config = mkIf cfg.enable {
  home.packages = with pkgs; [
    julia-bin
  ];
  };
}
