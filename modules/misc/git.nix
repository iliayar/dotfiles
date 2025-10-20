{ config, pkgs, lib, themes, ... }:

with lib;

let
  cfg = config.custom.misc;
in
{
  options = {
    custom.misc.git = {
      enable = mkOption {
        default = false;
      };

      gpg-key = mkOption {
        default = null;
        description = ''
          If not null, enables auto signing
        '';
      };

      save-creds = mkOption {
        default = false;
      };
    };
  };

  config = mkMerge [
    (mkIf (cfg.enable && cfg.git.enable) {
      programs.git = {
        enable = true;
        lfs.enable = true;

        userEmail = "iliayar3@gmail.com";
        userName = "iliayar";

        extraConfig = {
          core.editor = "vim";
          credential.helper = if cfg.git.save-creds then "store" else null;
        };

        aliases = {
          a = "add";
          co = "checkout";
          st = "status";
          br = "branch";
          ci = "commit";
        };

        ignores = [
          "*.~undo-tree~"
        ];

        delta.enable = true;
      };
    })
    (mkIf (cfg.enable && cfg.git.enable && cfg.gpg.enable && cfg.git.gpg-key != null) {
      programs.git = {
        signing = {
          signByDefault = true;
          key = cfg.git.gpg-key;
        };
      };
    })
  ];
}
