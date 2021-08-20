{ config, pkgs, ... }:

{
  imports = [
    ./xsession.nix
    ./xmonad
    ./xmobar
    ./browsers
    ./terminal-emulators
    ./fonts.nix
    ./sound.nix
  ];

  home.packages = with pkgs; [
    tdesktop # Telegram
  ];
}
