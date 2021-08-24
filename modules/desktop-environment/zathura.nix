{ pkgs, themes, ... }:

{
  programs.zathura = {
    enable = true;
    options = {
      recolor = "";
      recolor-keephue = "true";
      recolor-darkcolor = "${themes.foreground}";
      recolor-lightcolor = "${themes.background}";
    };
  };

}
