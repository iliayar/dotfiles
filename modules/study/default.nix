{ pkgs, ... }:
{
  imports = [
    ./sage.nix
  ];


  home.packages = with pkgs; [
    lci
    # xmind
    jetbrains.idea-community

    gnumake
  ];
}
