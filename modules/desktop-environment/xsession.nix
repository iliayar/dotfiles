{ config, pkgs, ... }:

{
  # Doesn't respect Xorg configs
  xsession = {
    enable = false;

    pointerCursor = {
      package = pkgs.vanilla-dmz;
      name = "Vanilla-DMZ";
      defaultCursor = "left_ptr";
      size = 16;
    };

    # initExtra = ''
    #           [ -e $HOME/.xprofile ] && . $HOME/.xprofile
    # '';
  };

}
