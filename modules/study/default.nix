{ pkgs, ... }:
{
  home.packages = with pkgs; [
    lci
    # xmind
    jetbrains.idea-community
  ];
}
