# Current build failed because of? missing C11 feature
{
  description = "Patched libXft for emoji fonts";

  inputs.nixpkgs.url = "nixpkgs/nixpkgs-unstable";

  outputs = { self, nixpkgs }:
    let
      supportedSystems = [ "x86_64-linux" ];
      forAllSystems = f: nixpkgs.lib.genAttrs supportedSystems (system: f system);
      nixpkgsFor = forAllSystems (system: import nixpkgs { inherit system; overlays = [ self.overlay ]; });
    in 
    {
      overlay = final: prev: {
        libxft = with final; stdenv.mkDerivation rec {
          name = "libxft";
          src = fetchFromGitHub {
            owner = "uditkarode";
            repo = "libxft-bgra";
            rev = "072cd202c0f4f757b32deac531586bc0429c8401";
            sha256 = "uYRdIcIGHvyohbwMU+Vx2ajTFfhN9jcBNlq0jOcg394=";
          };
          buildInputs = with nixpkgs.legacyPackages.${system}; [ xorg.utilmacros xorg.libX11 xorg.libXrender ];
          nativeBuildInputs = [ autoreconfHook ];
        };
      };

      packages = forAllSystems (system:
        {
          inherit (nixpkgsFor.${system}) libxft;

        }
      );

      defaultPackage = forAllSystems (system: self.packages.${system}.libxft);

    };
}
