{ system, pkgs, ... }:
let
  ghc = pkgs.ghc.withPackages (p: [
    p.xmonad
    p.xmonad-contrib
  ]);
in
pkgs.stdenv.mkDerivation {
  name = "xmonad-configured";
  src = ./.;

  buildPhase = ''
               ${ghc}/bin/ghc -ilib xmonad.hs -o xmonad-${system}
  '';

  installPhase = ''
               mkdir -p $out
               cp -r ./* $out/
  '';
}
