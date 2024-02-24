{ wezterm-newest, nixpkgs, pyprland-newest, system, ... }@inputs:
final: prev: {
  inkscape = (prev.inkscape.override {
    python3 = {
      withPackages = f:
        prev.python3.withPackages
        (pypkgs: with pypkgs; [ pygobject3 gst-python ] ++ (f pypkgs));
    };
  }).overrideAttrs (old: {
    buildInputs = old.buildInputs
      ++ (with prev; [ gobject-introspection gtk3 ]);
  });

  # pyprland = pyprland-newest.packages.${system}.pyprland;
}
