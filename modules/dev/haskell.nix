{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.custom.dev.haskell;
in
{
  options = {
    custom.dev.haskell = {
      enable = mkOption {
        default = false;
      };
    };
  };


  config = mkIf cfg.enable {
  home.packages = with pkgs; [
    (ghc.withPackages (hpkgs: with hpkgs; [
      hscolour
    ]))
  ];
  };
}
