{ config, lib, pkgs, ... }:

with lib;

let cfg = config.custom.dev.train;
in {
  options = {
    custom.dev.train = {
      enable = mkOption { default = false; };
    };
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      leetcode-cli
    ];
  };
}
