{ config, pkgs, lib, themes, ... }:

with lib;

let
  cfg = config.custom.de.picom;
in
{
  options = {
    custom.de.picom = {
      enable = mkOption {
        default = false;
      };
    };
  };

  config = mkIf cfg.enable {
    services.picom = {
      enable = true;
      package = pkgs.picom-next;
      vSync = true;
      settings = {
        inactive-dim = 0.3;
        backend = "glx";
      };

      opacityRules = [
        "80:class_g = 'xmobar'"
        "80:class_g = 'Pinentry'"
        "90:class_g = 'Steam'"
        "95:class_g = 'Emacs'"
        "80:class_g = 'Zathura'"
        "80:class_g = 'Pcmanfm'"
        "80:class_g = 'code-oss'"
        "90:class_g = 'Spotify'"
        "90:class_g = 'TelegramDesktop'"
        "90:class_g = 'cutter'"
        "90:class_g = 'jetbrains-idea'"
      ];
    };
  };
}
