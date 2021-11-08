{ pkgs, ... }:
{
  home.packages = with pkgs; [
    lci
    jetbrains.idea-community
  ];
}
