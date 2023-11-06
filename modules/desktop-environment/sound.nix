{ config, pkgs, lib, themes, ... }:

with lib;

let cfg = config.custom.de;
in {
  options = {
    custom.de.audio-utils = { enable = mkOption { default = false; }; };

    custom.de.easyeffects = {
      enable = mkOption { default = false; };
      service.enable = mkOption { default = false; };
    };
  };

  config = mkMerge [
    (mkIf cfg.audio-utils.enable {
      home.packages = with pkgs; [ pulseaudio pulsemixer paprefs ];
    })
    (mkIf cfg.easyeffects.enable {
      home.packages = with pkgs; [ easyeffects ];
    })
    (mkIf (cfg.easyeffects.enable && cfg.easyeffects.service.enable) {
      services.easyeffects.enable = true;
    })
  ];
}
