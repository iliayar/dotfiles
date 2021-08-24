{ libxft-bgra, picom-jonaburg, ... }@inputs:

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

  rust-blocks = import ../pkgs/rust-blocks { pkgs = prev; };
}
