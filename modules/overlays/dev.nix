{ ... }@inputs:
final: prev: {
  systec-can = prev.pkgs.callPackage ../dev/systec-socketcan.nix inputs;
}
