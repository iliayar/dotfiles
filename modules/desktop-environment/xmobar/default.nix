{ config, pkgs, lib, themes, ... }:

with lib;

let
  cfg = config.custom.de.xmobar;
in
{
  imports = [
    ./rust-blocks
  ];

  options = {
    custom.de.xmobar = {
      enable = mkOption {
        default = false;
      };

      rust-blocks = mkOption {
        default = true;
      };
    };
  };

  config = mkIf cfg.enable {
    home.packages = [ 
      (pkgs.haskellPackages.callPackage ./my-xmobar.nix { inherit themes; })
    ];
  };
}
