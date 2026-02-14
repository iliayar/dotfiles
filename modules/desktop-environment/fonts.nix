{ config, pkgs, lib, themes, ... }:

with lib;

let
  cfg = config.custom.de.fonts;
in
{
  options = {
    custom.de.fonts = {
      enable = mkOption {
        default = true;
      };
    };
  };

  config = mkIf cfg.enable {
    fonts = {
      fontconfig = {
        enable = true;
      };
    };

    xdg.configFile."fontconfig/conf.d/40-font-order.conf".text = ''
    <?xml version="1.0"?>
    <!DOCTYPE fontconfig SYSTEM "urn:fontconfig:fonts.dtd">
    <fontconfig>
      <alias>
              <family>sans-serif</family>
              <prefer>
                    <family>Ubuntu</family>
                    <family>Noto Color Emoji</family>
                    <family>DejaVu Sans</family>
              </prefer> 
          </alias>
          <alias>
              <family>serif</family>
              <prefer>
                    <family>Ubuntu</family>
                    <family>Noto Color Emoji</family>
                    <family>DejaVu Serif</family>
              </prefer>
          </alias>
          <alias>
              <family>monospace</family>
              <prefer>
                    <family>Ubuntu</family>
                    <family>Noto Color Emoji</family>
                    <family>DejaVu Sans Mono</family>
              </prefer>
          </alias>
    </fontconfig>
  '';

    home.packages = with pkgs; [
      noto-fonts-color-emoji
      fira-code
      font-awesome
      # symbola
      ubuntu-classic
      nerd-fonts.fira-code
      corefonts
      libertine
    ];
  };
}
