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

  config = mkIf cfg.brave.enable {
    home.sessionVariables = {
      BROWSER = "brave";
    };

    home.packages = with pkgs; [
      brave
    ];

    xdg.mimeApps = {
      defaultApplications = {
        "x-scheme-handler/http" = [ "brave-browser.desktop" ];
        "x-scheme-handler/https" = [ "brave-browser.desktop" ];
        "x-scheme-handler/about" = [ "brave-browser.desktop" ];
        "x-scheme-handler/unknown" = [ "brave-browser.desktop" ];
      };
    };
  };
}
