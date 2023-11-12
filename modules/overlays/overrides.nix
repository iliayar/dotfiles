{ xmonad-contrib-newest, xmonad-newest, wezterm-newest, nixpkgs, ... }@inputs:
final: prev: {
  haskellPackages = prev.haskellPackages.override (old: {
    overrides = final.lib.composeExtensions (old.overrides or (_: _: {}))
      (hfinal: hprev: {
        xmonad-contrib = xmonad-contrib-newest.defaultPackage."x86_64-linux";
        xmonad = xmonad-newest.defaultPackage."x86_64-linux";
      });
  });

  inkscape = (prev.inkscape.override {
    python3 = {
      withPackages = f: prev.python3.withPackages (pypkgs: with pypkgs; [
        pygobject3
        gst-python
      ] ++ (f pypkgs));
    };
  }).overrideAttrs (old: {
    buildInputs = old.buildInputs ++ (with prev; [
      gobject-introspection
      gtk3
    ]);
  });

  # FIXME: wezterm broken now
  wezterm = prev.wezterm.override {
    rustPlatform = prev.rustPlatform // {
      buildRustPackage = args:
        prev.rustPlatform.buildRustPackage (args // rec {
          src = wezterm-newest;
          cargoLock = {
            lockFile = "${src}/Cargo.lock";
            outputHashes = {
              "xcb-1.2.1" = "sha256-zkuW5ATix3WXBAj2hzum1MJ5JTX3+uVQ01R1vL6F1rY=";
              "xcb-imdkit-0.2.0" = "sha256-L+NKD0rsCk9bFABQF4FZi9YoqBHr4VAZeKAWgsaAegw=";
            };
          };
        });
    };
  };
}
