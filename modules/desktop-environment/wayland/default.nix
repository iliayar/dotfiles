{ config, lib, pkgs, ... }:
{
  imports = [
    ./sway.nix
    ./river.nix
    ./hyprland/hyprland.nix
    ./waybar.nix
  ];
}
