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
  swww = swww.packages.${system}.default;

  # FIXME: There is an unstable version! Check if it really works
  # wezterm = wezterm-newest.packages.${system}.default;

  # FIXME: Cleanup when fixed
  qt6Packages = prev.qt6Packages.overrideScope (_: kprev: {
    qt6gtk2 = kprev.qt6gtk2.overrideAttrs (_: {
      version = "0.5-unstable-2025-03-04";
      src = final.fetchFromGitLab {
        domain = "opencode.net";
        owner = "trialuser";
        repo = "qt6gtk2";
        rev = "d7c14bec2c7a3d2a37cde60ec059fc0ed4efee67";
        hash = "sha256-6xD0lBiGWC3PXFyM2JW16/sDwicw4kWSCnjnNwUT4PI=";
      };
    });
  });

  # zellij = prev.zellij.overrideDerivation (old: rec {
  #   version = "0.42.2";
  #   src = prev.fetchFromGitHub {
  #       owner = "zellij-org";
  #       repo = "zellij";
  #       rev = "v${version}";
  #       sha256 = "";
  #   };
  # });
}
