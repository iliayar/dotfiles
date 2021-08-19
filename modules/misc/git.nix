{ config, pkgs, ... }:

{
  programs.git = {
    enable = true;

    userEmail = "iliayar3@gmail.com";
    userName = "iliayar";

    extraConfig = {
      core.editor = "vim";
    };

    signing = {
      signByDefault = true;
      key = "0x3FE87CB13CB3AC4E";
    };
  };

}
