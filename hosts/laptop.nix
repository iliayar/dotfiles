{ pkgs, ... }:
{
  environment.sessionVariables = {
    "BROWSER" = "brave";
  };

  virtualisation.podman = {
    enable = true;
    dockerCompat = true;
  };
  users.users."iliayar".extraGroups = [ "dialout" ];

  environment.systemPackages = with pkgs; [
    # docker-compose
  ];

  # networking.bonds."bond0" = {
  #   interfaces = [ "wlp61s0" "enp0s20f0u2" ];
  #   driverOptions.mode = "802.3ad";
  # };
}
