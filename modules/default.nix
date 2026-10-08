{ config, pkgs, ... }:


{
  imports = [
    ./editors
    ./misc
    ./shell
    ./desktop-environment
    ./dev
    ./study
    ./ai
    ./settings.nix
    ./other.nix
  ];
}
