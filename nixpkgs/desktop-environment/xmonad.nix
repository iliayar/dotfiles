{ config, pkgs, ...}:

let
  dotfiles = (import ../config.nix).dotfiles;
in
{
  home.file.".xmonad" = {
    source = "${dotfiles}/.xmonad";
    recursive = true;
  };

  # FIXME: Defined in host/configuraion.nix
  xsession.windowManager.xmonad = {
    enable = false;
    enableContribAndExtras = true;
  };

}
