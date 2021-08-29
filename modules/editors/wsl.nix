{ config, pkgs, secrets, ... }:

{
  imports = [
    ./neovim
    ./wakatime.nix
  ];
}
