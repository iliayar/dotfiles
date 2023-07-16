{ config, pkgs, ... }:

{
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "iliayar";
  home.homeDirectory = "/home/iliayar";

  home.packages = with pkgs; [
    xournalpp
    pkgs.gnome.adwaita-icon-theme
  ];

  custom = {
  
    settings = {
      code-stats-machine = "DellLaptop";
    };

    dev = {
      python.enable = true;
      cpp.enable = true;
      latex.enable = true;

      embed.enable = true;

      uci.enable = true;
    };

    study.sage.enable = true;

    games = {
      minecraft.enable = true;
      wine.enable = true;
      lutris.enable = true;

      extra.enable = true;
    };

    editors.emacs = {
      enable = true;
      server = true;

      bundles = {
        code-stats.enable = true;
        evil-integrations.enable = true;
        web.enable = true;
      };

      misc = {
        enable = true;
        code = { enable = true; };
      };

      langs.enable = [ "nix" "python" "misc" "haskell" "rust" ];
    
      code-assist = { enable = true; };

      evil = { enable = true; };
    
      org = {
        roam = {
          enable = true;
          ui = true;
        };
      };

      pretty = {
        theme = "doom-gruvbox";
        extra.enable = true;
        font-size = 95;
      };

      packages = {
        ace-window.enable = true;
        avy.enable = true;
        hl-todo.enable = true;
        lsp-ui.enable = true;
      };
    };

    editors.nvim = {
      enable = true;
      code-stats = true;
      lsp = true;
    };

    misc = {
      enable = true;
      syncthing = true;

      git = {
        enable = true;
        gpg-key = "0x3FE87CB13CB3AC4E";
      };

      gpg.enable = true;
      pass.enable = true;
      ssh.enable = true;

      zellij.enable = true;

      net.enable = true;
    };

    shell = {
      misc.enable = true;
      zsh.enable = true;
    };

    de = {
      misc = true;
      media = true;
      social = true;

      browsers = {
        brave.enable = true;
        qute.enable = true;
      };

      terms = {
        alacritty.enable = true;
        # Moving to
        wezterm.enable = true;
        
        urxvt.enable = true;
      };

      # xmobar.enable = true;
      # xmonad.enable = true;

      # lock.enable = true;
      # conky.enable = true;
      dunst.enable = true;
      # picom.enable = true;

      audio-utils.enable = true;
      spotify.enable = true;

      zathura.enable = true;

      # pointer.enable = true;

      wayland.sway.enable = false;
      wayland.river.enable = false;
      wayland.hyprland.enable = true;
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
  home.stateVersion = "22.05";
}
