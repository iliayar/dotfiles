{ config, pkgs, ... }:

{
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "iliayar";
  home.homeDirectory = "/Users/iliayar";


  custom = {

    settings = {
      code-stats-machine = "Work";
    };

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
      enable = true;
      code-stats = true;
    };

    editors.emacs = {
      enable = true;

      bundles = {
        evil-integrations.enable = true;
        code-stats.enable = true;
      };

      misc = {
        enable = true;
        code = { enable = true; };
      };

      langs = {
        enable = [ "nix" "cpp" "python" "misc" "rust" ];
        cpp.ls = "clangd";
        python.ls = "pyright";
      };

      code-assist = { enable = true; };

      evil = { enable = true; };

      pretty = {
        theme = "doom-gruvbox";
        font-size = 120;
      };

      packages = {
        ace-window.enable = true;
        avy.enable = true;
        hl-todo.enable = true;
        lsp-ui.enable = true;
      };

      extraConfig = ''
        ; (setq ccls-initialization-options
        ; '(:compilationDatabaseDirectory "/Users/iliayar/projects-build/.vscode"
        ;             :cache (:directory "/Users/iliayar/projects-build/.vscode/.ccls-cache")))
        (setq lsp-clients-clangd-args '(
            "--background-index"
            "--compile-commands-dir=/Users/iliayar/projects-build/.vscode" 
            "--header-insertion=never" 
            "--log=info"
            "--pretty"
            "-j=12"
        ))

        (setq projectile-indexing-method 'native)
        (setq projectile-enable-caching t)
        (setq trash-directory "~/.trash")
      '';
    };

    de.terms.alacritty = {
      enable = true;
      option_as_alt = true;
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
