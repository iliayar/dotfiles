{ config, pkgs, ... }:


{
  imports = [
    ./editors
    ./misc
    ./shell
    ./desktop-environment
    ./dev
    ./study
    ./settings.nix
    ./other.nix
  ];
}
