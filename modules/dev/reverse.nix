{ pkgs, ... }:
{
  home.packages = with pkgs; [
    ghidra-bin
    cutter
    rz-ghidra
  ];
}
