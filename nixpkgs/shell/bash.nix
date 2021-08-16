{ config, pkgs, ... }:

{
  programs.bash = {
    enable = true;
    profileExtra = "zsh";
  };
}
