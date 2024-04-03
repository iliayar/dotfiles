{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.custom.de;

  zoom-fixed = pkgs.writeShellScriptBin "zoom-fixed" ''
    if [[ -z $WAYLAND_DISPLAY ]]; then
        zoom $@
    else
        QT_XCB_GL_INTEGRATION=xcb_egl QT_QPA_PLATFORM=xcb XDG_CURRENT_DESKTOP=GNOME ${pkgs.zoom-us}/bin/zoom $@
    fi
  '';
in {
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
    ./wayland
  ];

  options = {
    custom.de = {
      misc = mkOption { default = false; };

      media = mkOption { default = false; };

      social = {
        enable = mkOption { default = false; };
        fix-zoom-non-nixos = mkOption { default = false; };
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
        # tlpui

        scrot
        xclip
        slop
      ];

      home.sessionVariables = { TERM = "xterm-256color"; };

      programs = {
        feh = { enable = true; };

        autorandr = {
          enable = true;
          hooks = {
            postswitch = {
              "notify-xmonad" =
                "${pkgs.haskellPackages.xmonad}/bin/xmonad --restart";
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

      xdg.mimeApps = {
        enable = true;
        defaultApplications = {
          "image/png" = [ "feh.desktop" ];
          "image/jpeg" = [ "feh.desktop" ];
        };
      };
    })

    (mkIf cfg.media {
      home.packages = with pkgs; [ gimp vlc mpv deluge ];
      programs.obs-studio = {
        enable = true;
        plugins = with pkgs.obs-studio-plugins; [
            wlrobs
            obs-backgroundremoval
            obs-pipewire-audio-capture
        ];
      };
    })

    (mkIf cfg.social.enable {
      home.packages = with pkgs; [
        tdesktop
        (discord.override { withVencord = true; })
        (if cfg.social.fix-zoom-non-nixos then zoom-fixed else zoom-us)
      ];
      xdg.desktopEntries = {
        zoom = {
          name = "Zoom";
          exec = "zoom-fixed %U";
          type = "Application";
          terminal = false;
          mimeType = [
            "x-scheme-handler/zoommtg"
            "application/x-zoom"
            "x-scheme-handler/zoomus"
            "x-scheme-handler/zoomphonecall"
          ];
        };
      };

      xdg.mimeApps = {
        defaultApplications = {
          "x-scheme-handler/tg" = [ "telegramdesktop.desktop" ];
          "x-scheme-handler/zoommtg" = [ "zoom.desktop" ];
          "application/x-zoom" = [ "zoom.desktop" ];
          "x-scheme-handler/zoomus" = [ "zoom.desktop" ];
        };
      };
    })
  ];
}
