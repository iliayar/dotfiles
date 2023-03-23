{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.custom.settings;
in
{
  options = {
    custom.settings = {
      code-stats-machine = mkOption {
        type = types.str;
      };
    };
  };
}
