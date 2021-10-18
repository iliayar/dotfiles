{ mkDerivation, base, lib, xmobar, pkgs, themes, ... }:
let
  config = pkgs.writeText "theme" ''
module Theme where

color0     = "${themes.color0}"
color1     = "${themes.color1}"
color2     = "${themes.color2}"
color3     = "${themes.color3}"
color4     = "${themes.color4}"
color5     = "${themes.color5}"
color6     = "${themes.color6}"
color7     = "${themes.color7}"
color8     = "${themes.color8}"
color9     = "${themes.color9}"
color10    = "${themes.color10}"
color11    = "${themes.color11}"
color12    = "${themes.color12}"
color13    = "${themes.color13}"
color14    = "${themes.color14}"
color15    = "${themes.color15}"
background = "${themes.background}"
foreground = "${themes.foreground}"
cursor     = "${themes.cursor}"
  '';
in
mkDerivation rec {
  pname = "my-xmobar";
  version = "1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base xmobar ];
  preBuild = ''
    cp ${config} lib/Theme.hs
  '';
  license = lib.licenses.bsd3;
}
