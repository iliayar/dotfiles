{ config, pkgs, ... }:

{
  programs.password-store = {
    enable = true;
    settings = {
      PASSWORD_STORE_DIR = "~/.password-store";
      PASSWORD_STORE_KEY = "0x3FE87CB13CB3AC4E";
    };
  };
}
