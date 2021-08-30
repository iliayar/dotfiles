{ config, pkgs, ... }:

{
  programs.bash = {
    enable = true;
    profileExtra = "exec ${pkgs.zsh}/bin/zsh";
  };
}
