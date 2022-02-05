{ config, pkgs, ... }:

{
  programs.git = {
    enable = true;
    lfs.enable = true;

    userEmail = "iliayar3@gmail.com";
    userName = "iliayar";

    extraConfig = {
      core.editor = "vim";
    };

    signing = {
      signByDefault = true;
      key = "0x3FE87CB13CB3AC4E";
    };

    aliases = {
      a = "add";
      co = "checkout";
      st = "status";
      br = "branch";
      ci = "commit";
    };
  };

}
