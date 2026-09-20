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
      package = pkgs.pass.withExtensions (exts: with exts; [ pass-otp pass-update pass-import ]);
      enable = true;
      settings = {
        PASSWORD_STORE_DIR = "$HOME/.password-store";
        PASSWORD_STORE_KEY = "0x3FE87CB13CB3AC4E";
      };
    };

    programs.browserpass = {
        enable = true;
        browsers = [ "firefox" ];
    };

    home.packages = with pkgs; (if pkgs.stdenv.hostPlatform.isLinux then [
        tessen
    ] else []);

    # FIXME: Seems broken
    # services.password-store-sync = {
    #   enable = true;
    # };
  };
}
