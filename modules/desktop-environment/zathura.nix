{ pkgs, themes, ... }:

{
  programs.zathura = {
    enable = true;
    options = {
      recolor = "true";
      recolor-keephue = "true";
      recolor-darkcolor = "${themes.foreground}";
      recolor-lightcolor = "${themes.background}";
    };
  };

}
