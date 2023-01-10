{ config, pkgs, lib, themes, ... }:

with lib;

let
  cfg = config.custom.de.audio-utils;
in
{
  options = {
    custom.de.audio-utils = {
      enable = mkOption {
        default = false;
      };
    };
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [ 
      pulseaudio
      pulsemixer
      paprefs
    ];
  };
}
