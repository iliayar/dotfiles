{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.custom.dev.zig;
in
{
  options = {
    custom.dev.zig = {
      enable = mkOption {
        default = false;
      };
    };
  };


  config = mkIf cfg.enable {
    home.packages = with pkgs; [
        zig
        zls
    ];
  };
}
