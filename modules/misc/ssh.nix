{ config, pkgs, lib, themes, ... }:

with lib;

let
  cfg = config.custom.misc;
in
{
  options = {
    custom.misc.ssh = {
      enable = mkOption {
        default = false;
      };
    };
  };

  config = mkIf (cfg.enable && cfg.ssh.enable) {
    programs.ssh = {
      enable = true;
    };
  };
}
