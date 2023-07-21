{ .. }:
{
  networking = {
    nat = {
      enable = true;
      internalIPs = [ "192.168.2.0/24" ];
      externalInterface = "TODO-external";
    };

    interfaces."TODO-internal" = {
      ipv4.addresses = [
        {
          address = "192.168.2.1";
          prefixLength = 24;
        }
      ];
    };
  };

  services.dhcpd4 = {
    enable = true;
    interfaces = [ "TODO-internal" ];
    extraConfig = ''
      ddns-update-style none;
      one-lease-per-client true;

      subnet 192.168.2.0 netmask 255.255.255.0 {
        range 192.168.2.100 192.168.2.254;
        default-lease-time 86400;

        option subnet-mask 255.255.255.0;
        option broadcast-address 192.168.2.255;
        option routers 192.168.2.1;
        option domain-name-servers 1.1.1.1,8.8.8.8,8.8.4.4
      }
    '';
  };
}
