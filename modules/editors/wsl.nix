{ config, pkgs, secrets, ... }:

{
  imports = [
    ./neovim
    ./emacs
    ./wakatime.nix
  ];
}
