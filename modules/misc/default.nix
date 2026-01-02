{ config, pkgs, lib, themes, ... }:

with lib;

let
  cfg = config.custom.misc;
in
{
  imports = [
    ./vcs.nix
    ./ssh.nix
    ./gpg.nix
    ./password-store.nix
    ./mail.nix
    ./zellij
    ./net.nix
  ];

  options = {
    custom.misc = {
      enable = mkOption {
        default = false;
      };

      syncthing = mkOption {
        default = false;
      };

      udiskie = mkOption {
        default = false;
      };
    };
  };

  config = mkIf cfg.enable (mkMerge [
    {
      home.packages = with pkgs; [
        killall
        unzip
        zip
        pandoc
        poppler-utils
        htop
        bottom

        ripgrep
        fd
        bat
        procs
        sd
        dust
        tokei
        delta
        hurl
        jq
        parallel
        mprocs
        just
      ];
    }
    (mkIf cfg.syncthing {
      services.syncthing.enable = true;
    })
    (mkIf cfg.udiskie {
      services.udiskie = {
        enable = true;
        tray = "never";
      };
    })
  ]);
}
