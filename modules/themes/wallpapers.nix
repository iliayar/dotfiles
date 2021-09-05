{ pkgs, mylib }:
let
  fixNames = mylib.writePython3Bin "fixNames" (pypkgs: [ ]) ''
           import os
           for img in os.listdir():
               old = img
               new = img.split()[2]
               print(f'Moving "{old}" to "{new}"')
               os.rename(old, new) 
  '';
in
pkgs.stdenv.mkDerivation rec {
  pname = "wallpapers";
  version = "1.0.0";

  src = pkgs.fetchzip {
    url = "https://zip.imgur.com/046df2a9c34fc749924137e9a08a9417723e73fe60fd8570bda66ba74d26648d";
    sha256 = "sha256-sBydMnyK/f6Y/sVPIpVdkRLLuKePLE9XiObA8Qsw8JA=";
    extension = "zip";
    stripRoot = false;
  };

  buildInputs = with pkgs; [
    unzip
  ];

  installPhase = ''
               ${fixNames}/bin/fixNames
               ${pkgs.imagemagick}/bin/convert NM41MId.png -resize 512x512 Neofetch.png
               cp Rm1L6BN.png Emacs.png
               mkdir -p $out
               mv * $out/
  '';

}
