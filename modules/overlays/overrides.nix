{ my-xmonad-contrib, xmonad-newest, ... }@inputs:
final: prev: {
  haskellPackages = prev.haskellPackages.extend (hfinal: hprev: {
    xmonad-contrib = my-xmonad-contrib.defaultPackage."x86_64-linux";
    xmonad = xmonad-newest.defaultPackage."x86_64-linux";
  });
}
