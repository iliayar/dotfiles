{ config, pkgs, wallpapers, ... }:

{
  imports = [
    ./xsession.nix
    ./xmonad
    ./xmobar
    ./browsers
    ./terminal-emulators
    ./fonts.nix
    ./sound.nix
    ./dunst.nix
  ];

  home.packages = with pkgs; [
    tdesktop # Telegram
    spotify
    xkb-switch
    light
  ];

  programs = {
    feh = {
      enable = true;
    };
  };

  home.file."Wallpapers" = {
    source = wallpapers;
    recursive = true;
  };
}
