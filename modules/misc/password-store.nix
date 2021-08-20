{ config, pkgs, ... }:

{
  programs.password-store = {
    enable = true;
    settings = {
      PASSWORD_STORE_DIR = "$HOME/.password-store";
      PASSWORD_STORE_KEY = "0x3FE87CB13CB3AC4E";
    };
  };

  services.password-store-sync = {
    enable = true;
  };
}
