{ pkgs, ... }:
{
  environment.sessionVariables = {
    "BROWSER" = "brave";
  };

  # networking.bonds."bond0" = {
  #   interfaces = [ "wlp61s0" "enp0s20f0u2" ];
  #   driverOptions.mode = "802.3ad";
  # };
}
