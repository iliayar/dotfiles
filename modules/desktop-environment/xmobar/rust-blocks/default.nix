{ config, pkgs, lib, themes, ... }:

with lib;

let
  cfg = config.custom.de.xmobar;
in
{
  config = mkIf (cfg.enable && cfg.rust-blocks) {
    home.file.".emacs.d/lisp/load-org.el".source = ./load-org.el;

    home.packages = [ 
      pkgs.rust-blocks
    ];
  };
}
