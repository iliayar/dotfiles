{ pkgs, themes, ... }:
{
  services.stalonetray = {
    enable = false;
    config = {
      geometry = "5x1+20+20";
      icon_size = 20;
      transparent = true;
      sticky = false;
      window_type = "dock";
      window_strut = "auto";
      skip_taskbar = 1;
      icon_gravity = "NW";
      background = "${themes.background}";
    };
  };

}
