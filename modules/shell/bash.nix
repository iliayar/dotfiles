{ config, pkgs, ... }:

{
  programs.bash = {
    enable = true;
    profileExtra = "exec zsh";
  };
}
