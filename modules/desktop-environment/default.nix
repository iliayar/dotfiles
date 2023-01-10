{ config, lib, pkgs, wallpapers, ... }:

with lib;

let
  cfg = config.custom.de;
in
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

  options = {
    custom.de = {
      misc = mkOption {
        default = false;
      };

      media = mkOption {
        default = false;
      };

      social = mkOption {
        default = false;
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.misc {
      home.packages = with pkgs; [
        xkb-switch
        light
        nitrogen
        pcmanfm
        arandr
        tlpui

        scrot
        xclip
        slop
      ];

      home.sessionVariables = {
        TERM = "xterm-256color";
      };

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
          "image/png" = [ "feh.desktop" ];
          "image/jpeg" = [ "feh.desktop" ];
        };
      };
    })

    (mkIf cfg.media {
      home.packages = with pkgs; [
        gimp
        vlc
        mpv
        deluge
        obs-studio
      ];
    })

    (mkIf cfg.social {
      home.packages = with pkgs; [
        tdesktop
        discord
        slack
        zoom-us
      ];

      xdg.mimeApps = {
        defaultApplications = {
          "x-scheme-handler/tg" = [ "telegramdesktop.desktop" ];
        };
      };
    })
  ];
}
