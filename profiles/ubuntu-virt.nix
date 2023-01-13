{ config, pkgs, ... }:

{
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "iliayar";
  home.homeDirectory = "/home/iliayar";


  custom = {
    misc = {
      enable = true;
      git.enable = true;
    };

    shell = {
      misc.enable = true;
      zsh.enable = true;
    };

    editors.nvim = {
      enable = true;
      code-stats = false;
    };

    editors.emacs = {
      enable = true;
      evil = true;

      dicts = false;

      misc = {
        enable = true;
      };

      nix = true;
      exwm = true;

      code-misc = true;
      prog-misc = true;

      lsp = true;
      rust = true;
    };

    de.terms.alacritty.enable = true;
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
