{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  cfg = config.custom.dev.misc;
in
{
  options = {
    custom.dev.misc = {
      enable = mkOption { default = true; };
    };
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      markdownlint-cli
      codebook
    ];

    # NOTE: codebook writes words to config :(
    # xdg.configFile."codebook/codebook.toml".source = (pkgs.formats.toml { }).generate "codebook.toml" {
    #   dictionaries = [
    #     "en_US"
    #     "ru"
    #   ];
    # };
  };
}
