{ config, pkgs, ...}:

{
  home.file.".xmonad" = {
    source = ./.xmonad;
    recursive = true;
  };

  # home.file.".local/bin"

  xsession.windowManager.xmonad = {
    enable = false;
    enableContribAndExtras = true;
  };

  home.packages = with pkgs; [
    dzen2 
    gawk
  ];

}
