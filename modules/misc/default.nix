{ config, pkgs, lib, themes, ... }:

with lib;

let
  cfg = config.custom.misc;
in
{
  imports = [
    ./git.nix
    ./ssh.nix
    ./gpg.nix
    ./password-store.nix
    ./mail.nix
    ./zellij
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
        default = true;
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
        poppler_utils

        ripgrep
        fd
        bat
        procs
        sd
        du-dust
        tokei
        delta
        hurl
        jq
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
