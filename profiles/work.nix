{ config, pkgs, ... }:

{
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "iliayar";
  home.homeDirectory = "/Users/iliayar";

  denv = {
    langs.protobuf.enable = true;
    langs.nix.enable = true;
    langs.lua.enable = true;
  };

  custom = {

    hw.qmk.enable = true;

    settings = { code-stats-machine = "Work"; };

    misc = {
      enable = true;
      git.enable = true;
      zellij.enable = true;

      udiskie = false;
    };

    shell = {
      misc.enable = true;
      zsh = {
        enable = true;
        extra = ''
          [ -f ~/.tars.zsh ] && source ~/.tars.zsh
        '';
      };
    };

    editors.nvim = {
      bundles = { codeStats.enable = true; };

      enable = true;
      misc = {
        enable = true;
        code = { enable = true; };
      };
      langs.enable =
        [ "misc" "nix" "python" "rust" "go" "lua" "cpp" "protobuf" ];
      code-assist = { enable = true; };
      pretty = {
        status-bar.enable = true;
        todo-comments.enable = true;
      };

      langs.cpp.clangdCommand = [
        "clangd"
        "--background-index"
        "--header-insertion=never"
        "--log=info"
        "--pretty"
        "-j=12"
      ];
    };

    editors.emacs = {
      enable = true;
      package = pkgs.emacs29-macport;

      bundles = { code-stats.enable = true; };

      misc = { enable = true; };

      org = {
        roam.enable = true;
        reveal.enable = true;
        style = "v2";
      };

      evil = { enable = true; };

      pretty = {
        theme = "doom-gruvbox";
        font-size = 120;
      };

      packages = {
        ace-window.enable = true;
        avy.enable = true;
        hl-todo.enable = true;
      };
    };

    de.terms = {
      alacritty = {
        enable = true;
        option_as_alt = true;
      };

      wezterm = {
        enable = true;
        # FIXME: wezterm completely broken
        package = pkgs.hello;
        enableShellIntegration = false;
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
  home.stateVersion = "22.05";
}
