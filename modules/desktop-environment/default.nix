{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.custom.de;

  zoom-fixed = pkgs.writeShellScriptBin "zoom-fixed" ''
    if [[ -z $WAYLAND_DISPLAY ]]; then
        zoom $@
    else
        env -i HOME=$HOME WAYLAND_DISPLAY=$WAYLAND_DISPLAY DISPLAY=$DISPLAY XDG_SESSION_TYPE=$XDG_SESSION_TYPE DBUS_SESSION_BUS_ADDRESS=$DBUS_SESSION_BUS_ADDRESS XDG_CURRENT_DESKTOP=GNOME XDG_RUNTIME_DIR=$XDG_RUNTIME_DIR zoom $@
    fi
  '';
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
    ./wayland
  ];

  options = {
    custom.de = {
      misc = mkOption { default = false; };

      media = mkOption { default = false; };
      obs.enable = mkOption { default = false; };

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
        lxqt.pcmanfm-qt
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
        platformTheme.name = "gtk";
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
        # FIXME(iliayar): VLC sometimes crashes(
        mpv

        # deluge 
        transmission_4-qt
      ];
    })

    (mkIf cfg.obs.enable {
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
        vesktop
      ] ++ (if cfg.social.fix-zoom-non-nixos
        then [ zoom-fixed ]
        else [ zoom-us ]);

      xdg.mimeApps = {
        defaultApplications = {
          "x-scheme-handler/tg" = [ "telegramdesktop.desktop" ];
          "x-scheme-handler/zoommtg" = [ "zoom.desktop" ];
          "application/x-zoom" = [ "zoom.desktop" ];
          "x-scheme-handler/zoomus" = [ "zoom.desktop" ];
        };
      };
    })

    (mkIf (cfg.social.enable && cfg.social.fix-zoom-non-nixos) {
      home.packages = with pkgs; [
        zoom-fixed
      ];
      xdg.desktopEntries = {
        zoom = {
          name = "Zoom";
          exec =
            let
              zoom-bin =
                if cfg.social.fix-zoom-non-nixos then
                  "${zoom-fixed}/bin/zoom-fixed"
                else
                  "${zoom-fixed-nixos}/bin/zoom-fixed";
            in
            "${zoom-bin} %U";
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
    })
  ];
}
