{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.custom.de.browsers;
in
{
  imports = [
    ./brave.nix
  ];

  options = {
    custom.de.browsers.qute = {
      enable = mkOption {
        default = false;
      };
    };
  };

  config = mkIf cfg.qute.enable {
    home.packages = with pkgs; [
      qutebrowser
    ];
  };
}
