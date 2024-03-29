{ config, pkgs, ... }:

{
  imports = [
    ../../modules/shell/zsh/wsl.nix
    ../../modules/shell
    ../../modules/shell/bash.nix
    ../../modules/editors
    ../../modules/misc/git.nix
    ../../modules/dev/python
  ];

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "iliayar";
  home.homeDirectory = "/home/iliayar";

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  # home.stateVersion = "21.05";
}
