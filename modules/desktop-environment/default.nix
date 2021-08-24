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
    ./picom.nix
    ./auto-lock.nix
    ./zathura.nix
    ./stalonetray.nix
    ./conky.nix
    ./spotify.nix
  ];

  home.packages = with pkgs; [
    tdesktop # Telegram
    xkb-switch
    light
    rust-blocks
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

  xdg.mimeApps = {
    enable = true;
    defaultApplications = {
      "text/plain" = [ "emacsclient.desktop" ];
      "text/html" = [ "brave-browser.desktop" ];
      "x-scheme-handler/http" = [ "brave-browser.desktop" ];
      "x-scheme-handler/https" = [ "brave-browser.desktop" ];
      "x-scheme-handler/about" = [ "brave-browser.desktop" ];
      "x-scheme-handler/unknown" = [ "brave-browser.desktop" ];
      "x-scheme-handler/tg" = [ "telegramdesktop.desktop" ];
      "image/png" = [ "feh.desktop" ];
      "image/jpeg" = [ "feh.desktop" ];
      "application/pdf" = [ "zathura.desktop" ];
    };
  };
}
