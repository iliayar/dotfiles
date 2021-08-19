{ config, pkgs, ... }:


{
  imports = [
    ./editors
    ./misc
    ./shell
    ./desktop-environment
  ];

  home.sessionVariables = {
    EDITOR = "vim";
    VISUAL = "emacs";
    BROWSER = "brave";
  };
}
