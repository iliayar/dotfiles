{
  config,
  pkgs,
  lib,
  themes,
  ...
}:

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

      creds = {
        email = mkOption {
          default = "iliayar3@gmail.com";
        };
        name = mkOption {
          default = "iliayar";
        };
      };

      ignores = mkOption {
        default = [ ];
        type = types.listOf types.str;
      };
    };

    custom.misc.jujutsu = {
      enable = mkOption {
        default = false;
      };

      extraSettings = mkOption {
        default = { };
        example = {
          extraSettings = {
            templates = {
              commit_trailers = ''
                format_signed_off_by_trailer(self)
              '';
            };
          };
        };
      };
    };
  };

  config = mkMerge [
    (mkIf (cfg.enable && cfg.git.enable) {
      home.packages = with pkgs; [ git-filter-repo ];
      programs.git = {
        enable = true;
        lfs.enable = true;

        settings = {
          user.email = cfg.git.creds.email;
          user.name = cfg.git.creds.name;

          core.editor = "vim";
          credential.helper = if cfg.git.save-creds then "store" else false;

          alias = {
            a = "add";
            co = "checkout";
            st = "status";
            br = "branch";
            ci = "commit";
          };
        };

        ignores = [
          "*.~undo-tree~"
        ]
        ++ cfg.git.ignores;

      };
      programs.delta.enable = true;
    })
    (mkIf (cfg.enable && cfg.git.enable && cfg.gpg.enable && cfg.git.gpg-key != null) {
      programs.git = {
        signing = {
          signByDefault = true;
          key = cfg.git.gpg-key;
        };
      };
    })

    (mkIf (cfg.enable && cfg.jujutsu.enable) {
      programs.jujutsu = {
        enable = true;
        settings = {
          user = {
            email = cfg.git.creds.email;
            name = cfg.git.creds.name;
          };

          ui = {
            pager = [
              "less"
              "-RFX"
            ];
            conflict-marker-style = "git";
          };
        }
        // cfg.jujutsu.extraSettings;
      };
    })

    (mkIf (cfg.enable && cfg.jujutsu.enable && cfg.gpg.enable && cfg.git.gpg-key != null) {
      programs.jujutsu.settings = {
        signing = {
          behavior = "own";
          backend = "gpg";
          key = cfg.git.gpg-key;
        };
      };
    })
  ];
}
