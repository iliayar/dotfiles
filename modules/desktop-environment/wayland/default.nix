{ config, lib, pkgs, ... }:
{
  imports = [
    ./sway.nix
    ./river.nix
    ./hyprland.nix
    ./waybar.nix
  ];
}
