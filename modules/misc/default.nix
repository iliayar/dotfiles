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
    zip
    pandoc
    poppler_utils

    ripgrep
    fd
    bat
    procs
    sd
    du-dust
    tokei
    delta
  ];

  services.syncthing.enable = true;
}
