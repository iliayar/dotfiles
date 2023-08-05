{ config, pkgs, lib, themes, ... }:

with lib;

let cfg = config.custom.de;
in {
  options = {
    custom.de.audio-utils = { enable = mkOption { default = false; }; };

    custom.de.easyeffects = { enable = mkOption { default = false; }; };
  };

  config = mkMerge [
    (mkIf cfg.audio-utils.enable {
      home.packages = with pkgs; [ pulseaudio pulsemixer paprefs ];
    })
    (mkIf cfg.easyeffects.enable {
      services.easyeffects.enable = true;
      home.packages = with pkgs; [ easyeffects ];
    })
  ];
}
