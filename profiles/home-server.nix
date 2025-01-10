{ config, pkgs, ... }:

{
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "iliayar";
  home.homeDirectory = "/home/iliayar";

  custom = {
    de.fonts.enable = false;

    editors.nvim = {
      enable = true;

      bundles = {
        # codeStats.enable = true;
      };

      misc = {
        enable = true;
        code = {
          enable = true;
        };
      };
      langs.enable = [ "misc" "nix" ];
      code-assist = { enable = true; };
      pretty = { status-bar.enable = true; };
    };

    misc = {
      enable = true;

      git = { enable = true; };

      udiskie = false;
      pass.enable = true;
    };

    shell = {
      misc.enable = true;
      zsh.enable = true;
      tmux.enable = true;
    };

    dev = {
        latex.enable = true;
    };

    editors.emacs = {
      enable = true;
      server = false;

      bundles = {
        # code-stats.enable = true;
        evil-integrations.enable = true;
      };

      misc = {
        enable = true;
        code = { enable = true; };
      };

      langs.enable = [ "nix" "misc" "latex" ];

      code-assist = {
        enable = true;
        pretty.enable = true;
      };

      evil = { enable = true; };

      org = {
        roam = {
          enable = true;
        };
        style = "v2";
      };

      pretty = {
        theme = "doom-gruvbox";
        extra.enable = false;
        font-size = 120;
      };
    };
  };

  services.syncthing = {
    enable = true;
    guiAddress = "0.0.0.0:8384";
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
