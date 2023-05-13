{ config, lib, pkgs, ... }:

with lib;

let cfg = config.custom.dev.uci;
in {
  options = {
    custom.dev.uci = {
      enable = mkOption { default = false; };
      daemon = mkOption { default = false; };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable { home.packages = with pkgs; [ uci ]; })
    (mkIf (cfg.enable && cfg.daemon) { home.packages = with pkgs; [ ucid ]; })
  ];
}
