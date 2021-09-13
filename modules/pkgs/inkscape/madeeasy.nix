{ pkgs, ... }:
pkgs.stdenv.mkDerivation {
  name = "inkscape-madeeasy";

  src = pkgs.fetchFromGitHub {
    owner = "fsmMLK";
    repo = "inkscapeMadeEasy";
    rev = "c67c18d342c5dbeccb0ddbb898ab1707d11ef10c";
    sha256 = "sha256-AS3JXpK9fkqWkrJheNxkHkHU6+A7F2wMqnjokBAAWIQ=";
  };
  installPhase = ''
               mkdir -p $out/share/inkscape/extensions
               cp -r latest $out/share/inkscape/extensions/inkscapeMadeEasy
  '';
}
