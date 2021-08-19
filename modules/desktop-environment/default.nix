{ config, pkgs, ... }:

{
  imports = [
    ./xsession.nix
    ./xmonad
    ./browsers
    ./terminal-emulators
  ];
}
