{ config, pkgs, uci, system, ... }:

{
  imports = [ ./hardware-configuration.nix ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "NixOsSrv";
  networking.networkmanager.enable = true;

  time.timeZone = "Europe/Moscow";

  i18n.defaultLocale = "en_US.UTF-8";

  programs.zsh.enable = true;
  users.users.iliayar = {
    isNormalUser = true;
    extraGroups = [ "wheel" "docker" ];
    shell = pkgs.zsh;
  };


  environment.systemPackages = with pkgs; [ vim ];

  services.openssh = {
    enable = true;
    settings.PermitRootLogin = "yes";
  };

  users.users.root.openssh.authorizedKeys.keys = [
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQC8pnmPL9aFmXf7nzWxP9nDBFB5WC+hxdZcxXT+NTYm7G8UGGXf7G/F+FKIaHVCdUYidDVO4QVQtjG3qmQRjc52phWTXuk3bf1zGdvnZiriLRLeaXDRqQ+Ldm/UfGbQBU3yf9dih1DXsmp+R0gTkX+ZPAUyYz4/n7jSKs3bqHmzijVz348yajpYmo7UddozphPzBcf/Wxq/UqnGdmaQ3SepW88kCNEagDsN139pfOBXrdBaseBv5GBUhlio5QadRUoosEILOjc9oZqvZ5Nrxm/em8r/sPs/acSi9kTN2SqSMXSR5vRm24mQKBv1x+7hCKr4RwS+MUP3eKWLddiR9NmilOuprqgkdfNlPVmFrRIe9cdfaZ/5qz7IU70Dge3K6D+iWQGsInr0d6vdxsw2G0yxosk9yULJpSjRqUaFfq/q8CY1CvDZ9aJ9n4qMkg2PVgJizK+bk7WdqVfU5BWaFlHpHjI9UsIbU50of2xbgZ+Q237QNtab+9bXby89ld+wCFY/YUtE9BaWKoA3ENLNb6ygDohSSQQW6y5QTbNtoiXSzQUgTC/DThr5p+A8W9poxidxcL1U1KX4kNZ48HCKibbUNcZed3p90MX0rerjIS3npUMk/kw9QccNN0yNsqLj2a3mFBGU1r2upjbi5rsaQ1zrbUC/gqEO/rVIi+TF8Xpl6w== iliayar@nixos"
  ];

  networking = {
    nat = {
      enable = true;
      internalIPs = [ "192.168.2.0/24" ];
      externalInterface = "enp2s0";
    };

    interfaces."enp4s0" = {
      ipv4.addresses = [{
        address = "192.168.2.1";
        prefixLength = 24;
      }];
    };
  };

  nix = {
    settings = {
      trusted-public-keys =
        [ "dell.iliayar.net:Sg+bqjaEeFM/tS6hXxVIzbH72JPeDW4vh0z7tFMQII8=" ];
    };
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };

  networking.firewall = {
    enable = true;
    allowedTCPPorts = [
      22
      80
      443
      # TODO: Remove it
      3002
    ];
    allowedUDPPorts = [ 53 ];
  };

  services.kea.dhcp4 = {
    enable = true;
    settings = {
      interfaces-config = { interfaces = [ "enp4s0" ]; };
      subnet4 = [{
        id = 1;
        pools = [{ pool = "192.168.2.100 - 192.168.2.254"; }];
        subnet = "192.168.2.0/24";
        option-data = [{
          name = "routers";
          data = "192.168.2.1";
        }];
      }];
      option-data = [{
        name = "domain-name-servers";
        data = "1.1.1.1,8.8.8.8,8.8.4.4";
      }];
    };
  };

  virtualisation.docker.enable = true;
  users.users.ci = {
    isNormalUser = true;
    extraGroups = [ "docker" ];
    # FIXME: For caddy
    homeMode = "755";
  };

  users.users.ci.openssh.authorizedKeys.keys = [
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQC8pnmPL9aFmXf7nzWxP9nDBFB5WC+hxdZcxXT+NTYm7G8UGGXf7G/F+FKIaHVCdUYidDVO4QVQtjG3qmQRjc52phWTXuk3bf1zGdvnZiriLRLeaXDRqQ+Ldm/UfGbQBU3yf9dih1DXsmp+R0gTkX+ZPAUyYz4/n7jSKs3bqHmzijVz348yajpYmo7UddozphPzBcf/Wxq/UqnGdmaQ3SepW88kCNEagDsN139pfOBXrdBaseBv5GBUhlio5QadRUoosEILOjc9oZqvZ5Nrxm/em8r/sPs/acSi9kTN2SqSMXSR5vRm24mQKBv1x+7hCKr4RwS+MUP3eKWLddiR9NmilOuprqgkdfNlPVmFrRIe9cdfaZ/5qz7IU70Dge3K6D+iWQGsInr0d6vdxsw2G0yxosk9yULJpSjRqUaFfq/q8CY1CvDZ9aJ9n4qMkg2PVgJizK+bk7WdqVfU5BWaFlHpHjI9UsIbU50of2xbgZ+Q237QNtab+9bXby89ld+wCFY/YUtE9BaWKoA3ENLNb6ygDohSSQQW6y5QTbNtoiXSzQUgTC/DThr5p+A8W9poxidxcL1U1KX4kNZ48HCKibbUNcZed3p90MX0rerjIS3npUMk/kw9QccNN0yNsqLj2a3mFBGU1r2upjbi5rsaQ1zrbUC/gqEO/rVIi+TF8Xpl6w== iliayar@nixos"
  ];

  services.caddy = {
    enable = true;
  };
  # FIXME: Storing data in ci's home
  systemd.services.caddy.serviceConfig = {
    ProtectHome = pkgs.lib.mkForce "false";
  };

  system.stateVersion = "22.05"; # Did you read the comment?
}
