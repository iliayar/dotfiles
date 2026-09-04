{ nixpkgs, pyprland-newest, system, ... }@inputs:
final: prev: {
  pyprland = pyprland-newest.packages.${system}.pyprland;

  # bs-manager = final.callPackage (import ../pkgs/bs-manager) {};

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
}
