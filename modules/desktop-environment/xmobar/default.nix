{ config, pkgs, themes, ... }:
{
  home.packages = [ 
    (pkgs.haskellPackages.callPackage ./my-xmobar.nix { inherit themes; })
  ];
}
