{ system, pkgs }:
let
  ghc = pkgs.haskellPackages.ghcWithPackages (p: [
      p.xmonad
      p.xmonad-contrib
    ]);
in
pkgs.stdenv.mkDerivation {
  name = "xmonad-configured";
  src = ./.;
  nativeBuildDepends = [
    ghc
  ];

  buildPhase = ''
               ${ghc}/bin/ghc -ilib xmonad.hs -o xmonad-${system}
  '';

  installPhase = ''
               mkdir -p $out
               cp -r ./* $out/
  '';
}
