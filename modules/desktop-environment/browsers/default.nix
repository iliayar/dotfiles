{ config, pkgs, ... }:

{
  imports = [
    ./brave.nix
  ];

  home.packages = with pkgs; [
    qutebrowser
  ];
}
