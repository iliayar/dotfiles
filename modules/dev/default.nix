{ pkgs, ... }:

{
  imports = [
    ./python
    ./cxx
    ./latex.nix
    ./reverse.nix
    ./haskell.nix
    ./julia.nix
    ./embed.nix
    ./uci.nix
    ./train.nix
    ./js.nix
  ];

}
