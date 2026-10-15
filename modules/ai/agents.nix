{
  config,
  pkgs,
  lib,
  nix-ai-tools,
  revdiff,
  system,
  graphify,
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
        # TODO: graphify pi install
        graphify.packages.${system}.default
      ];

      custom.misc.git.ignores = [
        "graphify-out"
      ];
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
