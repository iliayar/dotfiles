{ config, pkgs, lib, themes, ... }:

with lib;

let
  cfg = config.custom.de.spotify;
in
{
  options = {
    custom.de.spotify = {
      enable = mkOption {
        default = false;
      };

      tui = mkOption {
        default = false;
      };

      daemon = mkOption {
        default = false;
      };
    };
  };

  config = mkIf cfg.enable (mkMerge [
    {
      home.packages = with pkgs; [
        spotify
      ];
    }

    (mkIf cfg.tui {
      home.packages = with pkgs; [
        spotify-tui
      ];

      xdg.configFile."spotify-tui/config.yml".text = ''
   theme:
     active: "251, 73, 52"
     banner: "251, 73, 52"
     error_border: "251, 73, 52"
     error_text: "251, 73, 52"
     hint: "250, 189, 47"
     hovered: "250, 189, 47"
     inactive: "102, 92, 84"
     playbar_background: "29, 32, 33"
     playbar_progress: "251, 73, 52"
     playbar_text: "213, 196, 161"
     selected: "184, 187, 38"
     text: "213, 196, 161"
  '';
    })

    (mkIf cfg.daemon {
      services.spotifyd = {
        enable = true;
        settings = {
          global = {
            username = "iliayar3@gmail.com";
            password_cmd = "${pkgs.pass}/bin/pass spotify.com/iliayar3@gmail.com";
            backend = "alsa";
            bitrate = 320;
            no_audio_cache = false;
            initial_volume = 80;
            volume_normalisation = true;
            normalisation_pregain = -10;
            zeroconf_port = 1234;
            device_type = "computer";
          };
        };
      };
    })
  ]);

}
