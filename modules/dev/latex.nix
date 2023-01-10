{ config, lib, pkgs, ... }:

with lib;

let
  texc = pkgs.writeShellScriptBin "texc" ''
       ${pkgs.texlive.combined.scheme-full}/bin/pdflatex -interaction=nonstopmode -shell-escape
  '';
  cfg = config.custom.dev.latex;
in
{
  options = {
    custom.dev.latex = {
      enable = mkOption {
        default = false;
      };
    };
  };


  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      texc
    ];

    programs.texlive = {
      enable = true;
      extraPackages = tpkgs: {
        inherit (tpkgs)
          scheme-full;
      };
    };
  };
}
