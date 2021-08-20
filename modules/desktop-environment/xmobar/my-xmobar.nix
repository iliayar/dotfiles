{ mkDerivation, base, lib, xmobar, pkgs, themes, ... }:
let
  config = pkgs.writeText "theme" ''
module Theme where

import qualified GHC.Word

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

ts_background, ts_extra :: GHC.Word.Word64
ts_node, ts_nodealt, ts_highlight :: (GHC.Word.Word64, GHC.Word.Word64)
ts_background   = 0x00272822
ts_node         = (0xfff9f8f5, 0xff1c1e1f)
ts_nodealt      = (0xfff9f8f5, 0xff272822)
ts_highlight    = (0xfff9f8f5, 0xff75715e)
ts_extra        = 0xfff8f8f2
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
