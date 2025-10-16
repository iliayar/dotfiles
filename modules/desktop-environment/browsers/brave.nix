{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.custom.de.browsers;
in
{
  options = {
    custom.de.browsers.brave = {
      enable = mkOption {
        default = false;
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.zen.enable {
      home.packages = with pkgs; [
        brave
      ];
    })
    (mkIf (cfg.default == "brave") {
      home.sessionVariables = {
        BROWSER = "brave";
      };

      xdg.mimeApps = {
        defaultApplications = {
          "x-scheme-handler/http" = [ "brave-browser.desktop" ];
          "x-scheme-handler/https" = [ "brave-browser.desktop" ];
          "x-scheme-handler/about" = [ "brave-browser.desktop" ];
          "x-scheme-handler/unknown" = [ "brave-browser.desktop" ];
          "text/html" = [ "brave-browser.desktop" ];
          "text/xml" = [ "brave-browser.desktop" ];
          "application/xhtml+xml" = [ "brave-browser.desktop" ];
          "application/vnd.mozilla.xul+xml" = [ "brave-browser.desktop" ];
        };
      };
    })
  ];
}
