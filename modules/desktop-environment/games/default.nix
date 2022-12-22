{ pkgs, ... }:

{
  imports = [
    ./minecraft.nix
  ];

  home.packages = with pkgs; [
    # steam
    # steamcmd
    steam-tui

    wineWowPackages.staging
    # (winetricks.override { wine = wineWowPackages.staging; })
    winetricks
    samba
    libkrb5

    lutris
  ];
}
