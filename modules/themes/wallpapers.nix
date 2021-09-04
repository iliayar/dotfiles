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
    url = "https://zip.imgur.com/4159f3f4483ce1314de008188d126b02c12f0e6e46e5f94d960382b0a91f808a";
    sha256 = "sha256-/NVKOT2UpZP19zVn9CeIWiZYvQVGr7t8hXkuuKN4/uo=";
    extension = "zip";
    stripRoot = false;
  };

  buildInputs = with pkgs; [
    unzip
  ];

  installPhase = ''
               ${fixNames}/bin/fixNames
               mv NM41MId.png Neofetch.png
               mkdir -p $out
               mv * $out/
  '';

}
