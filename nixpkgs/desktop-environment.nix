{ config, pkgs, ... }:

{
  imports = map (path: ./desktop-environment + "/${path}.nix") [
    "xsession"
    "xmonad"
    "terminal-emulators/alacritty"
  ];
}
