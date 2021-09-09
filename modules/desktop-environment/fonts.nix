{ config, pkgs, ... }:

{
  fonts = {
    fontconfig = {
      enable = true;
      # defaultFonts.emoji = [ "Noto Color Emoji" ];
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
    noto-fonts-emoji
    fira-code
    font-awesome
    symbola
    ubuntu_font_family
  ];
}
