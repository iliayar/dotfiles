{ pkgs, ... }:
{
  home.packages = with pkgs; [
    tectonic
  ];

  programs.texlive = {
    enable = true;
    extraPackages = tpkgs: {
      inherit (tpkgs)
        scheme-full;
    };
  };
}
