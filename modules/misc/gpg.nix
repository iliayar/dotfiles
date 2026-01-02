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

  config = mkMerge [
    (mkIf (cfg.enable && cfg.gpg.enable && pkgs.stdenv.isDarwin) {
      home.file.".gnupg/gpg-agent.conf".text = ''
        pinentry-program ${pkgs.pinentry_mac}/Applications/pinentry-mac.app/Contents/MacOS/pinentry-mac
      '';
    })
    (mkIf (cfg.enable && cfg.gpg.enable && pkgs.stdenv.isLinux) {
      home.packages = [ pkgs.pinentry-gtk2 pkgs.gcr ];

      services.gpg-agent = {
        enable = true;
        pinentry.package = pkgs.pinentry-gtk2;

        # defaultCacheTtl = 604800;
      };
    })
    (mkIf (cfg.enable && cfg.gpg.enable) {
      programs.gpg = {
        enable = true;
      };
    })
  ];
}
