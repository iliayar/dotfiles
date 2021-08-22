{ my-xmonad-contrib, ... }:
final: prev: {
  haskell = prev.haskell // {
    packages = prev.haskell.packages // {
      xmonad-contrib = my-xmonad-contrib;
    };
  };
}
