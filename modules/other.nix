{ config, pkgs, ... }:

with pkgs.lib;

let
  cfg = config.custom;
in
{
  options = {
    custom = {
      hw = {
        qmk.enable = mkOption { default = false; }; 
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.hw.qmk.enable {
      home.packages = with pkgs; [
        qmk
      ];
    })
  ];
}
