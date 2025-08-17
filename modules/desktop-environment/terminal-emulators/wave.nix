{ config, pkgs, lib, themes, ... }:

with lib;

let
  cfg = config.custom.de.terms;
in
{

  options = {
    custom.de.terms = {
      wave = {
        enable = mkOption {
          default = false;
        };
      };
    };
  };

  config = mkIf cfg.wave.enable {
    home.packages = with pkgs; [ waveterm ];
  };
}
