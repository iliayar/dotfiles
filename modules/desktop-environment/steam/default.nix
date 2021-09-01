{ pkgs, ... }:

{
  home.packages = with pkgs; [
    # steam
    steamcmd
    steam-tui
  ];
}
