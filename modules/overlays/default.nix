{ my-xmonad-contrib, ... }@inputs:
[
  (import ./overrides.nix inputs)
  (import ./dev.nix inputs)
  (import ./additional.nix inputs)
  # (my-xmonad-contrib.overlay)
] # ++ (my-xmonad-contrib.overlays)
