{ config, lib, pkgs, ...}:

with lib;

let
  cfg = config.custom.study;
in
{
  imports = [
    ./sage.nix
  ];

  options = {
    custom.study.lci = {
      enable = mkOption {
        default = false;
      };
    };

    custom.study.idea = {
      enable = mkOption {
        default = false;
      };
    };

    custom.study.misc = {
      enable = mkOption { default = false; };
    };
  };

  config = mkMerge [
    (mkIf cfg.lci.enable {
      home.packages = with pkgs; [
        lci
      ];
    })
    (mkIf cfg.idea.enable {
      home.packages = with pkgs; [
        jetbrains.idea-community
      ];
    })
    (mkIf cfg.misc.enable {
      home.packages = with pkgs; [
        nodePackages.mermaid-cli
      ];
    })
  ];
}
