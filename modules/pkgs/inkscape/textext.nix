{ pkgs, ... }:
pkgs.stdenv.mkDerivation {
  name = "inkscape-textext";

  src = pkgs.fetchFromGitHub {
    owner = "textext";
    repo = "textext";
    rev = "1.5.0";
    sha256 = "0257sd320fg29v726whyr7h6f96d3qqccjvjy0llgl6043wnnyk4";
  };

  nativeBuildInputs = with pkgs; [
    gobject-introspection
  ];

  buildInputs = with pkgs; [
    (python3.withPackages (pypkgs: with pypkgs; [
      pygobject3
      gst-python
      wrapPython
    ])) 
    gtk3
    inkscape
    texlive.combined.scheme-full
  ];

  patches = [
    ./textext.patch
  ];

  installPhase = ''
               python setup.py --inkscape-extensions-path $out/share/inkscape/extensions
  '';
}
