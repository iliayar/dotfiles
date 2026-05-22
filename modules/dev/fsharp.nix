{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  cfg = config.custom.dev.fsharp;
in
{
  options = {
    custom.dev.fsharp = {
      enable = mkOption { default = false; };
    };
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      fsautocomplete
      dotnet-sdk_9
    ];
  };
}
