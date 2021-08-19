{ config, pkgs, ... }:

{
  imports = [
    ./git.nix
    ./ssh.nix
  ];
}
