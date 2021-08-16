{ config, pkgs, ... }:

{
  imports = map (path: ./editors + "/${path}.nix") [
    "neovim"
  ];
}
