{ config, pkgs, ... }:
{
  home.packages = [ 
    (pkgs.haskellPackages.callPackage ./my-xmobar.nix { })
  ];
}
