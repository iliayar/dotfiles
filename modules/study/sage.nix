{ config, lib, pkgs, ...}:

with lib;

let
  cfg = config.custom.study.sage;
in
{
  options = {
    custom.study.sage = {
      enable = mkOption {
        default = false;
        description = ''
          Install sagemath including jupyter core
        '';
      };

      jupyter = mkOption {
        default = true;
        description = ''
          Enable python's jupyter
        '';
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      programs.sagemath.enable = true;
    })
    (mkIf cfg.jupyter {
      custom.dev.python.jupyter = true;
    })
  ];
}
