{ config, pkgs, lib, themes, ... }:

with lib;

let
  cfg = config.custom.shell.misc;
in
{
  imports = [
    ./zsh
  ];

  options = {
    custom.shell.misc = {
      enable = mkOption {
        default = false;
      };
    };
  };

  config = mkIf cfg.enable {
    programs.fzf = {
      enable = true;
    };

    programs.eza = {
      enable = true;
      enableAliases = true;
    };

    programs.direnv = {
      enable = true;
      nix-direnv = {
        enable = true;
      };
    };

    programs.zoxide = {
      enable = true;
    };
  };
}
