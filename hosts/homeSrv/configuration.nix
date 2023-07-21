{ config, pkgs, ... }:

{
  imports =
    [ 
      ./hardware-configuration.nix
    ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "NixOsSrv";
  networking.networkmanager.enable = true;

  time.timeZone = "Europe/Moscow";

  i18n.defaultLocale = "en_US.UTF-8";

  users.users.iliayar = {
    isNormalUser = true;
    extraGroups = [ "wheel" ];
    packages = with pkgs; [
      git
    ];
  };

  environment.systemPackages = with pkgs; [
    vim
  ];

  services.openssh.enable = true;

  networking = {
    nat = {
      enable = true;
      internalIPs = [ "192.168.2.0/24" ];
      externalInterface = "enp2s0";
    };

    interfaces."enp4s0" = {
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
    interfaces = [ "enp4s0" ];
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

  system.stateVersion = "22.05"; # Did you read the comment?
}

