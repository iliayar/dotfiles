{ pkgs, ... }:
{
  home.packages = with pkgs; [
    (ghc.withPackages (hpkgs: with hpkgs; [
    ]))
  ];
}
