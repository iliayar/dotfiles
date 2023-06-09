{ config, pkgs, lib, ... }:

with lib;

let
  cfg = config.custom.games;
in
{
  imports = [
    ./minecraft.nix
  ];

  options = {
    custom.games = {
      steam = {
        tui = mkOption {
          default = false;
        };
      };

      wine = {
        enable = mkOption {
          default = false;
        };
      };

      lutris = {
        enable = mkOption {
          default = false;
        };
      };

      extra = {
        enable = mkOption {
          default = false;
        };
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.steam.tui {
      home.packages = with pkgs; [
        steam-tui
      ];
    })

    (mkIf cfg.wine.enable {
      home.packages = with pkgs; [
        wineWowPackages.staging
        winetricks
        samba
        libkrb5
      ];
    })

    (mkIf cfg.lutris.enable {
      home.packages = with pkgs; [
        lutris
        # protonup-qt # TODO: Actualize nixpkgs revision
        # osu-lazer   # TODO: Actualize nixpkgs revision
      ];
    })

    (mkIf cfg.extra.enable {
      home.packages = with pkgs; [
        gamemode
        mangohud
      ];
    })
  ];
}
