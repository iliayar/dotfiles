{ pkgs, ... }:

{
  home.packages = with pkgs; [
    # steam
    # steamcmd
    steam-tui

    # wineWowPackages.staging
    # (winetricks.override { wine = wineWowPackages.staging; })
    samba
    libkrb5

    lutris
  ];
}
