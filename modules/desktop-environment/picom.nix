{ pkgs, ... }:

{
  services.picom = {
    enable = true;
    package = pkgs.picom-jonaburg;
    experimentalBackends = true;
    vSync = true;
    settings = {
      blur = true;
      fade = true;
      fade-delta = 5;
      blur-background-fixed = false;
      blur-background-exclude = [
       "class_g = 'Peek'"
       "class_g = 'slop'"
       "class_g = 'conky'"
      ];

      inactive-dim = 0.2;
      # use-ewmh-active-win = true;

      transition-length = 300;

      blur-method = "dual_kawase";
      blur-strength = 5;

      backend = "glx";
    };

    opacityRules = [
      "80:class_g = 'xmobar'"
      "80:class_g = 'Pinentry'"
      "80:class_g = 'Steam'"
      "90:class_g = 'Emacs'"
      "80:class_g = 'Zathura'"
      "80:class_g = 'Pcmanfm'"
      "80:class_g = 'code-oss'"
      "90:class_g = 'Spotify'"
      "90:class_g = 'TelegramDesktop'"
      "90:class_g = 'cutter'"
      "90:class_g = 'jetbrains-idea'"
    ];
  };

}
