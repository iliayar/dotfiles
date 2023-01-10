{ config, pkgs, lib, themes, ... }:

with lib;

let
  cfg = config.custom.misc;
in
{
  options = {
    custom.misc.gpg = {
      enable = mkOption {
        default = false;
      };
    };
  };

  config = mkIf (cfg.enable && cfg.gpg.enable) {
    home.packages = [ pkgs.pinentry.gtk2 pkgs.gcr ];

    programs.gpg = {
      enable = true;
    };

    services.gpg-agent = {
      enable = true;
      pinentryFlavor = "gtk2";
    };
  };
}
