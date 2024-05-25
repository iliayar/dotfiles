{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.custom.dev.nix;
in
{
  options = {
    custom.dev.nix = {
      enable = mkOption {
        default = false;
      };
    };
  };


  config = mkIf cfg.enable {
    home.packages = with pkgs; [
        nixfmt-classic
        nixd
    ];
  };
}
