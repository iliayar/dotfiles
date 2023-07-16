{ config, pkgs, lib, themes, ... }:

with lib;

let
  cfg = config.custom.misc.net;
in
{
  options = {
    custom.misc.net = {
      enable = mkOption {
        default = false;
      };
    };
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      wireguard-tools
    ];
  };
}
