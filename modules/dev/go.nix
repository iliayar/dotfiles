{ config, lib, pkgs, ... }:

with lib;

let cfg = config.custom.dev.go;
in {
  options = { custom.dev.go = { enable = mkOption { default = false; }; }; };

  config = mkIf cfg.enable { home.packages = with pkgs; [ go gopls ]; };
}
