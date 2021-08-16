{ config, pkgs, ... }:

{
  imports = map (path: ./shell + "/${path}.nix") [
    "zsh"
    "bash" # Need for run zsh
  ];
}
