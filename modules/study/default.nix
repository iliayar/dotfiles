{ pkgs, ... }:
{
  home.packages = with pkgs; [
    lci
    xmind
  ];
}
