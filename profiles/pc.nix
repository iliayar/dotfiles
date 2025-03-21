{ config, pkgs, ... }:

{
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "iliayar";
  home.homeDirectory = "/home/iliayar";

  home.packages = with pkgs; [
    # xournalpp
    # pkgs.gnome.adwaita-icon-theme
    # obsidian
    # aichat
    # graphviz
    # plantuml-c4

    # sonic-pi
    # pipewire.jack
    # qpwgraph

    deploy-rs

    # libreoffice
  ];

  denv = { langs.haskell.enable = true; };

  custom = {

    hw.qmk.enable = true;

    settings = { code-stats-machine = "DellLaptop"; };

    dev = {
        python.enable = true;
        go.enable = true;
        nix.enable = true;
        ocaml.enable = true;
        cpp.enable = true;
    };
    # dev = {
    #   python.enable = true;
    #   python.additionalPackages = pypkgs: with pypkgs; [ pygments ];
    #   cpp.enable = true;
    #   js.enable = true;
    #   latex.enable = true;
    #   rust.enable = true;
    #   nix.enable = true;
    #   lua.enable = true;
    #   go.enable = true;
    #   ocaml.enable = true;
    #   sql.enable = true;
    #   typst.enable = true;

    #   embed.enable = true;

    #   uci.enable = true;
    #   uci.daemon = true;

    #   train.enable = true;

    #   lean.enable = true;
    # };

    # study.sage.enable = true;
    # study.misc.enable = true;

    # games = {
    #   minecraft.enable = true;
    #   wine.enable = true;
    #   lutris.enable = true;

    #   extra.enable = true;
    # };
    games = {
      click-the-circles.enable = true;
      extra.enable = true;
    };

    editors.emacs = {
      enable = true;
      server = false;

      bundles = {
        code-stats.enable = true;
        evil-integrations.enable = true;
        proof-assist.enable = true;
        wayland.enable = true;
      };

      misc = {
        enable = true;
        code = { enable = true; };
      };

      langs.enable = [ "nix" "misc" ];

      code-assist = {
        enable = true;
        pretty.enable = true;
      };

      evil = { enable = true; };

      org = {
        roam = {
          enable = true;
          ui = true;
        };
        style = "v2";
      };

      pretty = {
        theme = "doom-gruvbox";
        extra.enable = false;
        font-size = 120;
      };
    };

    editors.nvim = {
      bundles = {
        codeStats.enable = true;
        # obsidian.enable = true;
        # orgmode.enable = true;
        # agi.enable = false;
      };

      enable = true;
      misc = {
        enable = true;
        code = { enable = true; };
      };
      langs.enable = [
        "misc"
        "nix"
        # "python"
        # "rust"
        "go"
        # "lua"
        "ocaml"
        # "sql"
        # "latex"
        # "cpp"
        # "typst"
        # "plantuml"
        "haskell"
        # "lean"
      ];
      langs.cpp.lsp = "ccls";
      code-assist = { enable = true; };
      pretty = { status-bar.enable = true; };
    };

    misc = {
      enable = true;
      syncthing = true;
      udiskie = true;

      git = {
        enable = true;
        gpg-key = "0x3FE87CB13CB3AC4E";
      };

      gpg.enable = true;
      pass.enable = true;
      ssh.enable = true;

      # zellij.enable = true;

      net.enable = true;
    };

    shell = {
      misc.enable = true;
      zsh.enable = true;
      tmux.enable = true;
    };

    de = {
      misc = true;
      media = true;
      # obs.enable = true;
      social.enable = true;

      browsers = {
        brave.enable = true;
        # qute.enable = true;
      };

      terms = {
        alacritty.enable = true;
        # Moving to
        wezterm = {
          enable = true;
          # useNvidia = false;
        };

        ghostty = {
            enable = true;
        };

        # urxvt.enable = true;
      };

      # xmobar.enable = true;
      # xmonad.enable = true;

      # lock.enable = true;
      # conky.enable = true;
      dunst.enable = true;
      # picom.enable = true;

      # audio-utils.enable = true;
      # easyeffects.enable = true;
      spotify.enable = true;

      # zathura.enable = true;

      # pointer.enable = true;

      wayland.hyprland = {
        enable = true;
        portals.enable = true;

        termCmd = "ghostty";

        cursor.hyprcursor = "Bibata-Original-Classic";
        cursor.xcursor = "Bibata-Original-Classic";
      };

      wayland.waybar = {
        enable = true;
        modules = {
          left = ms:
            with ms; [
              hyprland-submap
              hyprland-workspaces
              wlr-taskbar
              hyprland-window
            ];
          center = ms: with ms; [ mpris ];
          right = ms:
            with ms; [
              # network
              pulseaudio
              (disk {
                name = "root";
                path = "/";
              })
              (disk {
                name = "data";
                path = "/home/";
              })
              cpu
              memory
              # FIXME: Which sensor?
              # k10temp tctl probably
              # (temperature {
              #   thermal-zone = 0;
              # })
              battery
              hyprland-language
              clock
              tray
            ];
        };
      };
    };
  };

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "24.11";
}
