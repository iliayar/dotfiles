{ pkgs, ... }:

{
  imports = [
    ./python.nix
    ./cxx.nix
    ./latex.nix
    ./reverse.nix
    ./haskell.nix
    ./julia.nix
    ./embed.nix
    ./uci.nix
    ./train.nix
    ./js.nix
    ./rust.nix
    ./nix.nix
    ./lua.nix
    ./go.nix
    ./ocaml.nix
    ./sql.nix
    ./typst.nix
  ];

}
