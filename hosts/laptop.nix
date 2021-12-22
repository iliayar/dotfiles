{ pkgs, ... }:
{
  environment.sessionVariables = {
    "BROWSER" = "brave";
  };

  virtualisation.docker.enable = true;
  users.users."iliayar".extraGroups = [ "docker" ];

  environment.systemPackages = with pkgs; [
    docker-compose
  ];

  # networking.bonds."bond0" = {
  #   interfaces = [ "wlp61s0" "enp0s20f0u2" ];
  #   driverOptions.mode = "802.3ad";
  # };
}
