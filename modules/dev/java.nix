{ config, lib, pkgs, ... }:

with lib;

let cfg = config.custom.dev.java;
in {
  options = {
    custom.dev.java = {
      enable = mkOption { default = false; };
    };
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
        google-java-format
    ];
  };
}
