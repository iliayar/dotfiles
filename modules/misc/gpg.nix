{ config, pkgs, ... }:

{
  home.packages = [ pkgs.pinentry.qt pkgs.gcr ];

  programs.gpg = {
    enable = true;
  };

  services.gpg-agent = {
    enable = true;
    pinentryFlavor = "qt";
  };
}
