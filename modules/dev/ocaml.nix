{ config, lib, pkgs, ... }:

with lib;

let cfg = config.custom.dev.ocaml;
in {
  options = { custom.dev.ocaml = { enable = mkOption { default = false; }; }; };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [ 
        ocaml
        dune_3
        ocamlPackages.findlib
        ocamlPackages.utop
        ocamlPackages.odoc
        ocamlPackages.ocaml-lsp
        ocamlPackages.ocamlformat
    ];

    programs.opam = {
        enable = true;
    };
  };
}
