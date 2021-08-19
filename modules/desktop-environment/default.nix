{ config, pkgs, ... }:

{
  imports = [
    ./xsession.nix
    ./xmonad
    ./xmobar
    ./browsers
    ./terminal-emulators
    ./fonts.nix
  ];

  home.packages = with pkgs; [
    tdesktop # Telegram
  ];
}
