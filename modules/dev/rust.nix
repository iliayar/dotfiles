{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.custom.dev.rust;
in
{
  options = {
    custom.dev.rust = {
      enable = mkOption {
        default = false;
      };
    };
  };


  config = mkIf cfg.enable {
    home.packages = with pkgs; [
        rust-bin.stable.latest.default
        
        pkg-config
        openssl
    ];
  };
}
