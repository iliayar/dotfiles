{ config, pkgs, ... }:


{
  imports = [
    ./editors
    ./misc
    ./shell
    ./desktop-environment
    ./dev
  ];

  home.sessionVariables = {
    EDITOR = "vim";
    VISUAL = "emacs";
    BROWSER = "brave";
  };

  home.packages = with pkgs; [
    scrot
    xclip
    slop
  ];
}
