{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.custom.dev.ts;
in
{
  options = {
    custom.dev.ts = {
      enable = mkOption {
        default = false;
      };
    };
  };


  config = mkIf cfg.enable {
    home.packages = with pkgs; [
        nodePackages.typescript-language-server
    ];
  };
}
