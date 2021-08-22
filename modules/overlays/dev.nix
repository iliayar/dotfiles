{ ... }:
final: prev: {
  systec-can = prev.pkgs.callPackage ../dev/can/systec-socketcan.nix { };
}
