{ config, pkgs, ... }:

{
  home.packages = [ pkgs.pinentry.gtk2 pkgs.gcr ];

  programs.gpg = {
    enable = true;
  };

  services.gpg-agent = {
    enable = true;
    pinentryFlavor = "gtk2";
  };
}
