{ config, lib, pkgs, ... }:

with lib;

let cfg = config.custom.editors.vscode;
in {
  options = {
    custom.editors.vscode = {
      enable = mkOption { default = false; };

      package = mkOption { default = pkgs.vscodium; };
    };
  };

  config = mkIf cfg.enable { 
    programs.vscode = {
      enable = true;
      package = cfg.package;
    };
  };
}
