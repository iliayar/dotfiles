{ config, pkgs, ... }:

{
  imports = [
    ./git.nix
    ./ssh.nix
    ./gpg.nix
    ./password-store.nix
  ];

  home.packages = with pkgs; [
    killall
  ];
}
