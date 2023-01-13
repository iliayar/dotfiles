{ system, pkgs, themes, ... }:
let
  ghc = pkgs.ghc.withPackages (p: [
    p.xmonad
    p.xmonad-contrib
    p.split
  ]);
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
ts_background   = ${themes.tsTransparent.black}
ts_node         = (${themes.ts.brightWhite}, ${themes.ts.background})
ts_nodealt      = (${themes.ts.brightWhite}, ${themes.ts.black})
ts_highlight    = (${themes.ts.brightWhite}, ${themes.ts.brightBlack})
ts_extra        = ${themes.ts.white}
  '';
in
pkgs.stdenv.mkDerivation {
  name = "xmonad-configured";
  src = ./.;

  buildPhase = ''
               cp ${config} lib/Theme.hs
               ${ghc}/bin/ghc -ilib xmonad.hs -o xmonad-${system}
               ${ghc}/bin/ghc -ilib xmonadctl.hs -o xmonadctl
  '';

  installPhase = ''
               mkdir -p $out
               cp -r ./* $out/
  '';
}
