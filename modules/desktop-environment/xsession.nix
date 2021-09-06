{ config, pkgs, ... }:

{
  xsession = {
    enable = true;

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
