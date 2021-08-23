{ pkgs, ... }:

{
  services.picom = {
    enable = true;
    package = pkgs.picom-jonaburg;
    experimentalBackends = true;
    blur = true;
    fade = true;
    fadeDelta = 5;
    inactiveDim = "0.2";
    extraOptions = ''
                 use-ewmh-active-win = true;

                 detect-rounded-corners = true;
                 corner-radius=8;
                 round-borders=8;

                 transition-length=300;

                 blur-method = "dual_kawase";
                 blur-strength = 5;
    '';
    opacityRule = [
      "80:class_g = 'xmobar'"
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
