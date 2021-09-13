{ pkgs, ... }:
pkgs.stdenv.mkDerivation {
  name = "inkscape-plot";

  src = pkgs.fetchFromGitHub {
    owner = "fsmMLK";
    repo = "inkscapeCartesianPlotData2D";
    rev = "c913d55ea7d0b5fa5d6d2a4306c181a0c683e47e";
    sha256 = "sha256-Pwib67koqC7HLENCRaz3j9bmZpbhFJ27/aTkTZdQUoU=";
  };
  installPhase = ''
               mkdir -p $out/share/inkscape/extensions
               cp -r latest $out/share/inkscape/extensions/cartesianPlotData2D
  '';
}
