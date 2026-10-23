{
  config,
  lib,
  pkgs,
  cjls,
  system,
  ...
}:

with lib;

let
  cfg = config.custom.dev.cangjie;
in
{
  options = {
    custom.dev.cangjie = {
      enable = mkOption { default = false; };
    };
  };

  config = mkIf cfg.enable { home.packages = with pkgs; [ cjls.packages.${system}.default ]; };
}
