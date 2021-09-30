{ pkgs, ... }:
let
  texc = pkgs.writeShellScriptBin "texc" ''
       ${pkgs.texlive.combined.scheme-full}/bin/pdflatex -interaction=nonstopmode -shell-escape
  '';
in
{
  home.packages = with pkgs; [
    tectonic
    texc
  ];

  programs.texlive = {
    enable = true;
    extraPackages = tpkgs: {
      inherit (tpkgs)
        scheme-full;
    };
  };
}
