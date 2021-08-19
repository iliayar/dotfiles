{ config, pkgs, ... }:

{
  imports = [
    ./xsession.nix
    ./xmonad
    ./browsers
    ./terminal-emulators
    ./fonts.nix
  ];

  home.packages = with pkgs; [
    tdesktop # Telegram
  ];
}
