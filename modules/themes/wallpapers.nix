{
  pkgs ? import <nixpkgs> {}
}:
pkgs.stdenv.mkDerivation rec {
  pname = "wallpapers";
  version = "1.0.0";

  src = pkgs.fetchzip {
    url = "https://imgur.com/a/eptG3El/zip";
    sha256 = "0ddk26bbkcwbf10y0qdrizh4bpar88hq8qsxcy52y1xv5ib3f0f0";
    extension = "zip";
    stripRoot = false;
  };

  buildInputs = with pkgs; [
    unzip
  ];

  installPhase = ''
               mv "11 - FRiMiAg.png" Neofetch.png
               mkdir -p $out
               mv * $out/
  '';

}
