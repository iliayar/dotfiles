{ config, pkgs, ... }:

{
  programs.git = {
    enable = true;

    userEmail = "iliayar3@gmail.com";
    userName = "iliayar";

    extraConfig = {
      core.editor = "vim";
    };
  };

}
