{ config, pkgs, ... }:

{
  imports = [
    ./git.nix
    ./ssh.nix
    ./gpg.nix
    ./password-store.nix
    ./mail.nix
  ];

  home.packages = with pkgs; [
    killall
    unzip
  ];

  services.syncthing.enable = true;
}
