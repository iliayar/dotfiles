{ config, pkgs, ... }:

{
  imports = [
    ./alacritty.nix
    ./urxvt.nix
    ./wezterm.nix
  ];
}
