{ pkgs, ... }:

{
  imports = [
    ./python
    ./cxx
    ./latex.nix
    ./reverse.nix
    ./haskell.nix
    ./julia.nix
  ];

}
