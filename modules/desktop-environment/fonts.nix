{ config, pkgs, ... }:

{
  fonts = {
    fontconfig = {
      enable = true;
      # defaultFonts.emoji = [ "Noto Color Emoji" ];
    };
  };

  home.packages = with pkgs; [
    noto-fonts-emoji
    fira-code
    font-awesome
    symbola
  ];
}
