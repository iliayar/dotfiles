{ config, lib, pkgs, ... }:

with lib;

let cfg = config.custom.dev.lua;
in {
  options = {
    custom.dev.lua = {
      enable = mkOption { default = false; };
    };
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
        stylua
    ];
  };
}
