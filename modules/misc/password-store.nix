{ config, pkgs, lib, themes, ... }:

with lib;

let
  cfg = config.custom.misc;
in
{
  options = {
    custom.misc.pass = {
      enable = mkOption {
        default = false;
      };
    };
  };

  config = mkIf (cfg.enable && cfg.pass.enable) {
    programs.password-store = {
      enable = true;
      settings = {
        PASSWORD_STORE_DIR = "$HOME/.password-store";
        PASSWORD_STORE_KEY = "0x3FE87CB13CB3AC4E";
      };
    };

    # FIXME: Seems broken
    # services.password-store-sync = {
    #   enable = true;
    # };
  };
}
