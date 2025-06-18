{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.custom.de.browsers;
in
{
  imports = [
    ./brave.nix
    ./zen.nix
  ];

  options = {
    custom.de.browsers.qute = {
      enable = mkOption {
        default = false;
      };
    };
    custom.de.browsers.default = mkOption {
        default = "brave";
        type = types.enum [ "none" "brave" "zen" ];
    };
  };

  config = mkIf cfg.qute.enable {
    home.packages = with pkgs; [
      qutebrowser
    ];
  };
}
