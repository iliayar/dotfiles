{ config, pkgs, ... }:

{
  imports = [
    ./xsession.nix
    ./xmonad
    ./browsers
    ./terminal-emulators
  ];

  home.packages = with pkgs; [
    tdesktop # Telegram
  ];
}
