{ config, pkgs, ... }:

{
  imports = [
    ./alacritty.nix
    ./urxvt.nix
  ];
}
