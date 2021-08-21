{ config, pkgs, ... }:

{
  imports = [
    ./zsh
    # ./bash.nix # Need for run zsh. UPD: No more
  ];

  programs.fzf = {
    enable = true;
  };

  programs.exa = {
    enable = true;
    enableAliases = true;
  };
}