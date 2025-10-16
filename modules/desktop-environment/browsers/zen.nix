{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.custom.de.browsers;
in
{
  options = {
    custom.de.browsers.zen = {
      enable = mkOption {
        default = false;
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.zen.enable {
      home.packages = with pkgs; [
        zen-browser
      ];
    })
    (mkIf (cfg.default == "zen") {
      home.sessionVariables = {
        BROWSER = "zen";
      };

      xdg.mimeApps = {
        defaultApplications = {
          "x-scheme-handler/http" = [ "zen.desktop" ];
          "x-scheme-handler/https" = [ "zen.desktop" ];
          "x-scheme-handler/about" = [ "zen.desktop" ];
          "x-scheme-handler/unknown" = [ "zen.desktop" ];
          "text/html" = [ "zen.desktop" ];
          "text/xml" = [ "zen.desktop" ];
          "application/xhtml+xml" = [ "zen.desktop" ];
          "application/vnd.mozilla.xul+xml" = [ "zen.desktop" ];
        };
      };
    })
  ];
}
