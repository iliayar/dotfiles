{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.custom.dev.typst;
in
{
  options = {
    custom.dev.typst = {
      enable = mkOption {
        default = false;
      };
    };
  };


  config = mkIf cfg.enable {
    home.packages = with pkgs; [
        typst
        typstfmt
        typst-lsp
        typst-live
    ];
  };
}
