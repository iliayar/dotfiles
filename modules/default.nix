{ config, pkgs, ... }:


{
  imports = [
    ./editors
    ./misc
    ./shell
    ./desktop-environment
    ./dev
    ./study
  ];

  home.sessionVariables = {
    EDITOR = "vim";
    VISUAL = "emacs";
    BROWSER = "brave";
    TERM = "xterm-256color";
  };

  home.packages = with pkgs; [
    scrot
    xclip
    slop
  ];
}
