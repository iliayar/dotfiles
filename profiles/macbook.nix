{ config, pkgs, nix-ai-tools, ... }:

{
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "iliayar";
  home.homeDirectory = "/Users/iliayar";

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

    # TODO: Move these somewhere
    # wireshark
    # libreoffice
    # deploy-rs

    # TODO: this one too
    cfcli

    # TODO: And this one
    yazi
    # meli
    # w3m
    dos2unix

    # thunderbird

    # yt-dlp
    # audacity

    # ani-cli
    mpv
  ];

  denv = { langs.haskell.enable = true; };

  # custom.de.fonts.enable = false;

  custom = {

    study.sage.enable = false;

    hw.qmk.enable = true;

    settings = { code-stats-machine = "MacBook"; };

    dev = {
      python.enable = true;
      go.enable = true;
      nix.enable = true;
      ocaml.enable = true;
      cpp.enable = true;
      # rust.enable = true;
      # zig.enable = true;

      # uci.enable = true;
      # uci.daemon = false;

      latex.enable = true;
      typst.enable = true;

      lean.enable = true;
      fsharp.enable = true;
    };

    editors.emacs = {
      enable = true;

      bundles = {
        code-stats.enable = true;
        evil-integrations.enable = true;
        # proof-assist.enable = true;
        # wayland.enable = true;
      };

      misc = {
        enable = true;
        code = { enable = true; };
      };

      langs.enable = [ "misc" "nix" "latex" ];

      code-assist = {
        enable = true;
        pretty.enable = true;
      };

      evil = { enable = true; };

      org = {
        # roam = {
        #   enable = true;
        #   ui = true;
        # };
        style = "v2";
      };

      pretty = {
        theme = "alabaster";
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
        # sonicpi.enable = true;
        # strudel.enable = true;
      };

      enable = true;
      misc = {
        enable = true;
        code = { enable = true; };
        debugger.enable = true;
      };
      langs.enable = [
        "misc"
        "nix"
        "python"
        "rust"
        "go"
        # "lua"
        "ocaml"
        # "sql"
        "latex"
        "cpp"
        "typst"
        # "plantuml"
        "haskell"
        "lean"
        "zig"
        "fsharp"
        "cangjie"
        "java"
      ];
      langs.cpp.lsp = "clangd";
      code-assist = { enable = true; };
      pretty = {
        status-bar.enable = true;
        theme = "gruvbox";
      };

      obsidian = {
        enable = true;
        path = "~/org/obsidian";
      };
    };

    misc = {
      enable = true;
      syncthing = true;
      # udiskie = true;

      git = {
        enable = true;
        gpg-key = "0x3FE87CB13CB3AC4E";
      };
      
      jujutsu.enable = true;

      gpg.enable = true;
      pass.enable = true;
      ssh.enable = true;

      zellij.enable = true;

      # net.enable = true;
    };

    shell = {
      misc.enable = true;
      zsh.enable = true;
      # tmux.enable = true;
    };

    de.terms.ghostty = {
      enable = true;
    };
    de.terms.alacritty.enable = true;

    ai = {
        enable = true;
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
  home.stateVersion = "25.05";
}
