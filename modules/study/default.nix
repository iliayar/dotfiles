{ pkgs, ... }:
{
  home.packages = with pkgs; [
    lci
  ];
}
