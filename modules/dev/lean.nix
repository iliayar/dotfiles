{ config, lib, pkgs, ... }:

with lib;

let cfg = config.custom.dev.lean;
in {
  options = { custom.dev.lean = { enable = mkOption { default = false; }; }; };

  config = mkIf cfg.enable { home.packages = with pkgs; [ lean4 ]; };
}
