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
  # swww = swww.packages.${system}.default;

  # FIXME: There is an unstable version! Check if it really works
  # wezterm = wezterm-newest.packages.${system}.default;

  # FIXME: Cleanup when fixed
  # qt6Packages = prev.qt6Packages.overrideScope (_: kprev: {
  #   qt6gtk2 = kprev.qt6gtk2.overrideAttrs (_: {
  #     version = "0.5-unstable-2025-03-04";
  #     src = final.fetchFromGitLab {
  #       domain = "opencode.net";
  #       owner = "trialuser";
  #       repo = "qt6gtk2";
  #       rev = "d7c14bec2c7a3d2a37cde60ec059fc0ed4efee67";
  #       hash = "sha256-6xD0lBiGWC3PXFyM2JW16/sDwicw4kWSCnjnNwUT4PI=";
  #     };
  #   });
  # });

  # FIXME: Cleanup when fixed
  hyprlandPlugins.hy3 = prev.hyprlandPlugins.hy3.overrideAttrs (_: {
    version = "0.52.0-50643d625829f303239f9ec019021f17d7e9f3fc";
    src = final.fetchFromGitHub {
      owner = "outfoxxed";
      repo = "hy3";
      rev = "50643d625829f303239f9ec019021f17d7e9f3fc";
      hash = "sha256-Wuk2MULnGfgAbP1N/Rp6H6k7zG/ynDiy54mPvkt/4bM=";
    };
  });

  bs-manager = final.callPackage (import ../pkgs/bs-manager) {};

  # bs-manager = prev.bs-manager.overrideAttrs (_: rec {
  #   version = "1.5.4-8edfea66f2717e9d1b6a78f941b755cfbe84d44a";
  #   src = final.fetchFromGitHub {
  #     owner = "Zagrios";
  #     repo = "bs-manager";
  #     rev = "8edfea66f2717e9d1b6a78f941b755cfbe84d44a";
  #     hash = "sha256-vXT+RrROUJB97ANNNsfNC+mco7tNtYvdsA/+pfXDO1E=";
  #   };
  #
  #   npmDepsHash = final.lib.fakeHash;
  #   # extraNpmDeps = fetchNpmDeps {
  #   #   name = "bs-manager-${finalAttrs.version}-extra-npm-deps";
  #   #   inherit src;
  #   #   sourceRoot = "${finalAttrs.src.name}/release/app";
  #   #   hash = "sha256-UWsxty1kfxMr5fybtykrN2G+yiQ9dw/bbMwfcVLJgp4=";
  #   # };
  #   extraNpmDeps = final.fetchNpmDeps {
  #       name = "bs-manager-${version}-extra-npm-deps";
  #       inherit src;
  #       # sourceRoot = "${src.name}/release/app";
  #       hash = "sha256-nv8QnX4tZDJ0HfREwMew5FNpDpG7KboxCo9HMi+BjaA=";
  #   };
  # });

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
