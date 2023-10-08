{ config, lib, pkgs, ... }:

with lib;

let cfg = config.custom.dev.cpp;
in {
  options = {
    custom.dev.cpp = {
      enable = mkOption { default = false; };

      compiler = mkOption {
        default = "clang";
        type = types.enum [ "clang" "gcc" ];
      };
    };
  };

  config = mkIf cfg.enable (mkMerge [
    (mkIf (cfg.compiler == "clang") {
      home.packages = with pkgs; [
        clang
        clang-tools
      ];
    })
    (mkIf (cfg.compiler == "gcc") {
      home.packages = with pkgs; [
        gcc
        ccls
      ];
    })
  ]);
}
