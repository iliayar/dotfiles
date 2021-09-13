{ my-xmonad-contrib, xmonad-newest, ... }@inputs:
final: prev: {
  haskellPackages = prev.haskellPackages.override (old: {
    overrides = final.lib.composeExtensions (old.overrides or (_: _: {}))
      (hfinal: hprev: {
        xmonad-contrib = my-xmonad-contrib.defaultPackage."x86_64-linux";
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
}
