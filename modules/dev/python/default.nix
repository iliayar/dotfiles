{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.custom.dev.python;
in
{
  options = {
    custom.dev.python = {
      enable = mkOption {
        default = true;
        description = ''
          Systemwide python environment
        '';
      };

      python = mkOption {
        default = pkgs.python3;
        description = ''
          Python package
        '';
      };

      web = mkOption {
        default = true;
        description = ''
          Enable web related packages:
          - requests
        '';
      };

      math = mkOption {
        default = true;
        description = ''
          Enable math related pacakges:
          - numpy
        '';
      };

      jupyter = mkOption {
        default = false;
        description = ''
          Enable jupyter related stuff
        '';
      };

      additionalPackages = mkOption {
        default = (ps: with ps; [
          ipython
        ]);
        description = ''
          additional python packages to include in systemwide installation
        '';
        type = mkOptionType {
          name = "Python packages function";
          merge = (loc: defs: 
            foldr ({ value, ... }: a: (ps: (value ps) ++ (a ps))) (ps: []) defs
          );
        };
      };
    };
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      (python3.withPackages (ps: with ps; 
        (if cfg.jupyter then [ jupyter ] else []) ++
        (if cfg.math then [ numpy ] else []) ++
        (if cfg.web then [ requests ] else []) ++
        (cfg.additionalPackages ps)
      ))
    ];
  };

}
