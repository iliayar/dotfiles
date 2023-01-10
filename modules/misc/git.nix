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
    };
  };

  config = mkIf (cfg.enable && cfg.git.enable) {
    programs.git = {
      enable = true;
      lfs.enable = true;

      userEmail = "iliayar3@gmail.com";
      userName = "iliayar";

      extraConfig = {
        core.editor = "vim";
      };

      signing = {
        signByDefault = true;
        key = "0x3FE87CB13CB3AC4E";
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
  };
}
