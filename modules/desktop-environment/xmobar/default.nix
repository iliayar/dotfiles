{ config, pkgs, themes, ... }:
{
  imports = [
    ./rust-blocks
  ];

  home.packages = [ 
    (pkgs.haskellPackages.callPackage ./my-xmobar.nix { inherit themes; })
  ];
}
