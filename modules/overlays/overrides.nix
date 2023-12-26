{ wezterm-newest, nixpkgs, pyprland, system, ... }@inputs:
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

  pyprland = pyprland.packages.${system}.default;

  # FIXME: wezterm broken now
  wezterm-fixed = prev.wezterm.override {
    rustPlatform = prev.rustPlatform // {
      buildRustPackage = args:
        prev.rustPlatform.buildRustPackage (args // rec {
          src = wezterm-newest;
          cargoLock = {
            lockFile = "${src}/Cargo.lock";
            outputHashes = {
              "xcb-1.2.1" =
                "sha256-zkuW5ATix3WXBAj2hzum1MJ5JTX3+uVQ01R1vL6F1rY=";
              "xcb-imdkit-0.2.0" =
                "sha256-L+NKD0rsCk9bFABQF4FZi9YoqBHr4VAZeKAWgsaAegw=";
            };
          };
        });
    };
  };

  obsidian =
    prev.lib.throwIf (prev.lib.versionOlder "1.4.16" prev.obsidian.version)
    "Obsidian no longer requires EOL Electron" (prev.obsidian.override {
      electron = prev.electron_27.overrideAttrs (_: {
        preFixup =
          "patchelf --add-needed ${prev.libglvnd}/lib/libEGL.so.1 $out/bin/electron"; # NixOS/nixpkgs#272912
        meta.knownVulnerabilities = [ ]; # NixOS/nixpkgs#273611
      });
    });
}
