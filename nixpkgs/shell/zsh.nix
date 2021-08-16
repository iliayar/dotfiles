{ config, pkgs, ... }:

let
  dotfiles = (import ../config.nix).dotfiles;
in
{
  programs.zsh = {
    enable = true;
    enableAutosuggestions = true;
    # enableSyntaxHighlighting = true;

    oh-my-zsh = {
      enable = true;
      plugins = [
        "git"
      ];

      custom = "${dotfiles}/home/.oh-my-zsh/themes";
      # custom = "\$HOME/Repos/dotfiles/home/.oh-my-zsh/themes";
      theme = "l";
    };
  };
}
