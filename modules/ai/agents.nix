{
  config,
  pkgs,
  lib,
  nix-ai-tools,
  revdiff,
  system,
  codegraph,
  ...
}:

with lib;

let
  cfg = config.custom.ai;
in
{
  options = {
    custom.ai = {
      enable = mkOption {
        default = false;
      };

      claude = {
        enable = mkOption {
          default = true;
        };
      };

      omp = {
        enable = mkOption {
          default = true;
        };
      };

      herdr = {
        enable = mkOption {
          default = true;
        };

        wherdrProxy = {
          enable = mkOption {
            default = false;
          };
          hostName = mkOption {
            type = types.str;
          };
        };
      };

      terminal-browser = {
        enable = mkOption {
          default = true;
        };
      };

      ollama = {
        enable = mkOption {
          default = false;
        };
      };
    };
  };

  config = mkIf cfg.enable (mkMerge [
    {
      home.packages = [
        revdiff.packages.${system}.default
      ];
    }
    (mkIf cfg.claude.enable {
      home.packages = [
        nix-ai-tools.packages.${system}.claude-code
      ];
    })
    (mkIf cfg.omp.enable {
      home.packages = with pkgs; [
        # TODO: Install plugins from here:
        #  - omp install https://github.com/umputun/revdiff
        nix-ai-tools.packages.${system}.omp
        bun

      ] ++ [
        codegraph.packages.${system}.default
      ];

      custom.misc.git.ignores = [
        ".codegraph"
      ];
    })
    (mkIf cfg.herdr.enable (
      let
        herdr = nix-ai-tools.packages.${system}.herdr;
      in
      {
        home.packages = [
          herdr
          pkgs.herdr-nvim
        ];

        home.activation.herdrPlugins = hm.dag.entryAfter [ "writeBoundary" ] ''
          run ${herdr}/bin/herdr plugin unlink chmarax.herdr-nvim >/dev/null 2>&1 || true
          run ${herdr}/bin/herdr plugin link ${pkgs.herdr-nvim}/share/herdr-nvim >/dev/null
        '';
      }
    ))
    (mkIf (cfg.herdr.enable && cfg.herdr.wherdrProxy.enable) {
      custom.misc.caddy = {
        enable = true;
        virtualHosts.${cfg.herdr.wherdrProxy.hostName}.upstream = "127.0.0.1:7683";
      };
    })
    (mkIf cfg.terminal-browser.enable {
      home.packages = [ pkgs.terminal-browser ];
    })
    (mkIf cfg.ollama.enable {
      services.ollama = {
        enable = true;
        acceleration = "rocm";
        environmentVariables = {
          HSA_OVERRIDE_GFX_VERSION = "11.0.0";
        };
      };

    })
  ]);
}
