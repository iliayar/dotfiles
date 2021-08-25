{ system, pkgs, ... }:
let
  ghc = pkgs.ghc.withPackages (p: [
    p.xmonad
    p.xmonad-contrib
    p.split
  ]);
in
pkgs.stdenv.mkDerivation {
  name = "xmonad-configured";
  src = ./.;

  buildPhase = ''
               ${ghc}/bin/ghc -ilib xmonad.hs -o xmonad-${system}
               ${ghc}/bin/ghc -ilib xmonadctl.hs -o xmonadctl
  '';

  installPhase = ''
               mkdir -p $out
               cp -r ./* $out/
  '';
}
