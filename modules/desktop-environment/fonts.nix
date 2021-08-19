{ config, pkgs, ... }:

{
  fonts = {
    fontconfig.enable = true;
  };

  home.packages = with pkgs; [
    noto-fonts-emoji
    fira-code
  ];
}
