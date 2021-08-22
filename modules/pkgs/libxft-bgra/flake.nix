# Current build failed because of? missing C11 feature
{
  description = "Patched libXft for emoji fonts";

  inputs = {
    nixpkgs.url = "nixpkgs/nixpkgs-unstable";

    libxft-bgra = {
      url = "github:uditkarode/libxft-bgra";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, libxft-bgra }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };
      stdenv = pkgs.stdenv;
    in
      {
        packages.${system} = {
          libxft-bgra = stdenv.
        };

        defaultPackage.${system} = self.packages.${system}.libxft-bgra;

      };
}
