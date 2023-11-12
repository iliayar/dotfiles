{ system, libxft-bgra, picom-jonaburg, wakatime-cli, tlpui-src, rust-blocks, uci, nwg-displays, ... }@inputs:

final: prev: {
  libxft-bgra = prev.stdenv.mkDerivation {
    name = "libxft-bgra";
    src = libxft-bgra;
    buildInputs = with prev; [ fontconfig xorg.utilmacros pkgconfig xorg.libX11 xorg.libXext xorg.libXrender ];
    nativeBuildInputs = with prev; [ freetype autoreconfHook ];
  };

  picom-jonaburg = prev.picom.overrideDerivation (old: {
    src = picom-jonaburg;
  });

  rust-blocks = rust-blocks.defaultPackage.${system};

  uci = uci.packages.${system}.uci;
  ucid = uci.packages.${system}.ucid;

  # wakatime-cli = prev.buildGoModule { 
  #   pname = "wakatime-cli";
  #   src = wakatime-cli;
  #   vendorSha256 = null;
  # };
  wakatime-cli = prev.wakatime;

  # rz-ghidra = prev.callPackage ../pkgs/rz-ghidra { pkgs = prev; };

  inkscape-extensions = prev.inkscape-extensions // {
    textext = prev.callPackage ../pkgs/inkscape/textext.nix { };
    madeeasy = prev.callPackage ../pkgs/inkscape/madeeasy.nix { };
    plot = prev.callPackage ../pkgs/inkscape/plot.nix { };
  };

  tlpui = prev.callPackage ../pkgs/tlpui { inherit tlpui-src; };

  nwg-displays = nwg-displays.packages.${system}.default;
}
