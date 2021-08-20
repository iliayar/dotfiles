{
  pkgs ? import <nixpkgs> {}
}:
pkgs.stdenv.mkDerivation rec {
  pname = "wallpapers";
  version = "1.0.0";

  src = pkgs.fetchurl {
    url = "https://imgur.com/a/eptG3El/zip";
    sha256 = "0R/6sSpfIvtn/XOpYhbpujuU4oFUDOR3BW2OTQbTFgg=";
  };

  buildInputs = with pkgs; [
    unzip
  ];

  phases = [ "unpackPhase" "installPhase" ];

  unpackPhase = ''
              mkdir wall
              unzip $src -d wall
  '';

  installPhase = ''
               mv "wall/11 - FRiMiAg.png" wall/Neofetch.png
               mkdir -p $out
               mv wall/* $out/
  '';

}
