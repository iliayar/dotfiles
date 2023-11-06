{ config, lib, pkgs, ... }:

with lib;

let cfg = config.custom.dev.sql;
in {
  options = {
    custom.dev.sql = {
      enable = mkOption { default = false; };
    };
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
        pgformatter
    ];
  };
}
