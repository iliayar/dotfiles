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
    ./games
  ];

  home.packages = with pkgs; [
    tdesktop # Telegram
    discord
    slack
    xkb-switch
    light
    rust-blocks
    gimp
    zoom-us
    vlc
    mpv
    # (inkscape-with-extensions.override {
      # inkscapeExtensions = with inkscape-extensions; [
        # textext
        # madeeasy
        # plot
      # ];
    # })
    # libreoffice
    deluge

    paprefs
    nitrogen
    pcmanfm

    arandr
    tlpui
  ];

  programs = {
    feh = {
      enable = true;
    };

    autorandr = {
      enable = true;
      hooks = {
        postswitch = {
          "notify-xmonad" = "${pkgs.haskellPackages.xmonad}/bin/xmonad --restart";
        };
      };
    };
  };

  gtk = {
    enable = true;
    theme = {
      package = pkgs.vimix-gtk-themes;
      name = "vimix-dark";
    };
  };

  qt = {
    enable = true;
    platformTheme = "gtk";
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
      "application/pdf" = [ "org.pwmt.zathura-pdf-mupdf.desktop" ];
    };
  };
}
