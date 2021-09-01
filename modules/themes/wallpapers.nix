{
  pkgs ? import <nixpkgs> {}
}:
pkgs.stdenv.mkDerivation rec {
  pname = "wallpapers";
  version = "1.0.0";

  src = pkgs.fetchurl {
    url = "https://imgur.com/a/eptG3El/zip";
    sha256 = "0z38zv4p5q0zhaisq6gcvavl1ny5ha4gbd7kd49d3zx0y6wjhkbi";
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
