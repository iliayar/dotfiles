{ config, pkgs, ... }:

{
  # Doesn't respect Xorg configs
  xsession = {
    enable = false;

    # initExtra = ''
    #           [ -e $HOME/.xprofile ] && . $HOME/.xprofile
    # '';
  };

  home.pointerCursor = {
    package = pkgs.vanilla-dmz;
    name = "Vanilla-DMZ";
    # defaultCursor = "left_ptr";
    size = 16;
    x11.enable = true;
  };

}
