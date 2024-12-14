{ wezterm-newest, nixpkgs, pyprland-newest, system, pyprland-my, swww, ... }@inputs:
final: prev: {
  # inkscape = (prev.inkscape.override {
  #   python3 = {
  #     withPackages = f:
  #       prev.python3.withPackages
  #       (pypkgs: with pypkgs; [ pygobject3 gst-python ] ++ (f pypkgs));
  #   };
  # }).overrideAttrs (old: {
  #   buildInputs = old.buildInputs
  #     ++ (with prev; [ gobject-introspection gtk3 ]);
  # });

  pyprland = pyprland-my.packages.${system}.pyprland;
  wezterm = wezterm-newest.packages.${system}.default;
  swww = swww.packages.${system}.default;
}
