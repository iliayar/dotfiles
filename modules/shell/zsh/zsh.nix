{ config, pkgs, ... }:

{
  programs.zsh = {
    enable = true;
    enableAutosuggestions = true;
    enableSyntaxHighlighting = true;

    oh-my-zsh = {
      enable = true;
      plugins = [
        "git"
      ];

      custom = "${./.oh-my-zsh/themes}";
      theme = "l";
    };
  };
}
