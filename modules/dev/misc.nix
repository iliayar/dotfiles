{ config, lib, pkgs, ... }:

with lib;

let cfg = config.custom.dev.misc;
in {
  options = {
    custom.dev.misc = {
      enable = mkOption { default = true; };
    };
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
        markdownlint-cli
    ];
  };
}
