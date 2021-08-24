{ config, pkgs, secrets, ... }:

let
  nvidia-offload = pkgs.writeShellScriptBin "nvidia-offload" ''
    export __NV_PRIME_RENDER_OFFLOAD=1
    export __NV_PRIME_RENDER_OFFLOAD_PROVIDER=NVIDIA-G0
    export __GLX_VENDOR_LIBRARY_NAME=nvidia
    export __VK_LAYER_NV_optimus=NVIDIA_only
    exec -a "$0" "$@"
  '';
in
{
  imports =
    [
      ./hardware-configuration.nix
    ];

  boot.supportedFilesystems = [ "ntfs" ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  time.timeZone = "Europe/Moscow";

  hardware.pulseaudio.enable = true;

  networking = {
    hostName = "NixLaptop";
    useDHCP = false;
    interfaces = {
      enp60s0.useDHCP = true;
      wlp61s0.useDHCP = true;
    }; 
    networkmanager.enable = true;

    firewall = {
      allowedUDPPorts = [ 41007 ];
    };

    # wireguard.enable = false;
    # wireguard.interfaces = {
    #   wg0 = {
    #     ips = [ "192.168.66.2/32" ];
    #     listenPort = 41007;
    #     privateKeyFile = "${secrets.wireguard.nixDell.private}";

    #     peers = [
    #       {
    #         publicKey = secrets.wireguard.server.public;
    #         allowedIPs = [ "0.0.0.0/0" ];
    #         endpoint = "vps.iliayar.ru:8999";
    #         persistentKeepalive = 25;
    #       }
    #     ];
    #   };
    # };
  };

  i18n = {
    defaultLocale = "en_US.UTF-8";
    supportedLocales = [
      "en_US.UTF-8/UTF-8"
      "ru_RU.UTF-8/UTF-8"
    ];
  };


  services.xserver = {
    enable = true;
    videoDrivers = [ "nvidia" ];
    screenSection = ''
      Option         "metamodes" "nvidia-auto-select +0+0 {ForceFullCompositionPipeline=On}"
      Option         "AllowIndirectGLXProtocol" "off"
      Option         "TripleBuffer" "on"
    '';
    displayManager.lightdm.enable = true;
    # FIXME: Move to nixpkgs folder somehow
    windowManager.xmonad = {
      enable = true;
    };
    synaptics = {
      enable = true;
      tapButtons = false;
      vertTwoFingerScroll = true;
      horizTwoFingerScroll = true;
      minSpeed = "1.0";
      maxSpeed = "3.0";
    };

    autoRepeatInterval = 50;
    autoRepeatDelay = 200;

    xkbOptions = "grp:switch,grp:alt_caps_toggle";
    layout = "us,ru";
  }; 

  hardware.nvidia.prime = {
    offload.enable = true;
    intelBusId = "PCI:0:2:0";
    nvidiaBusId = "PCI:1:0:0";
  };

  nix = {
    package = pkgs.nixFlakes;
    trustedUsers = [ "root" ];
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };  

  users.users.iliayar = {
    isNormalUser = true;
    home = "/home/iliayar";
    shell = pkgs.zsh;
    extraGroups = [ "wheel" "networkmanager" "video" ];
  };

  environment.systemPackages = with pkgs; [
    vim
    efibootmgr
    refind
    wget
    pciutils
    nvidia-offload
  ];

  security.sudo = {
    enable = true;
    extraConfig = ''
      Defaults pwfeedback
    '';
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  # system.stateVersion = "21.05"; # Did you read the comment?

}

