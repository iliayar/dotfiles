{ pkgs, lib, ... }:
let
  logidConfig = pkgs.writeText "logidConfig" ''
    devices: ({
      name: "Wireless Mouse MX Master 3";

      smartshift: { on: false; threshold: 50; };
      hiresscroll: { hires: true; invert: true; target: false; };
      dpi: 1500;
    
      buttons: (
        { cid: 0x53; action = { type: "Keypress"; keys: ["KEY_BACK"]; }; },
        { cid: 0x56; action = { type: "Keypress"; keys: ["KEY_FORWARD"];    }; }
      );
    });
  '';
in
{
  environment.sessionVariables = {
    "BROWSER" = "brave";
  };

  # virtualisation.podman = {
    # enable = true;
    # dockerCompat = true;
    # dockerSocket.enable = true;
  # };
  virtualisation.docker = {
    enable = true;
  };

  users.users."iliayar".extraGroups = [ "dialout" "docker" "audio" ];

  environment.systemPackages = with pkgs; [
    docker-compose
    # podman-compose
  ];

  # hardware.logitech.wireless.enable = true;
  # hardware.logitech.wireless.enableGraphical = true;

  systemd.services.logid = {
    serviceConfig = {
      ExecStart = ''
        ${pkgs.logiops}/bin/logid -c ${logidConfig}
      '';
      Restart = "always";
    };
    wantedBy = [ "default.target" ];
  };

  services.udisks2.enable = true;
}
