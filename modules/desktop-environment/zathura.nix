{ config, pkgs, lib, themes, ... }:

with lib;

let
  cfg = config.custom.de.zathura;
in
{
  options = {
    custom.de.zathura = {
      enable = mkOption {
        default = false;
      };
    };
  };

  config = mkIf cfg.enable {
    programs.zathura = {
      enable = true;
      options = {
        recolor = "true";
        recolor-keephue = "true";
        recolor-darkcolor = "${themes.foreground}";
        recolor-lightcolor = "${themes.background}";
      };
    };

    xdg.mimeApps = {
      defaultApplications = {
        "application/pdf" = [ "org.pwmt.zathura-pdf-mupdf.desktop" ];
      };
    };
  };
}
