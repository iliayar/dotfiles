{ config, pkgs, haskellPackages, ... }:

{
  imports =
    [
      ./hardware-configuration.nix
    ];

  boot.supportedFilesystems = [ "ntfs" ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  time.timeZone = "Europe/Moscow";

  networking = {
    hostName = "NixLaptop";
    useDHCP = false;
    interfaces = {
      enp60s0.useDHCP = true;
      wlp61s0.useDHCP = true;
    }; 
    networkmanager.enable = true;
  };


  services.xserver = {
    enable = true;
    displayManager.lightdm.enable = true;
    # FIXME: Move to nixpkgs folder somehow
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      extraPackages = gpkgs: [
        pkgs.ghc
      ];
    };
    synaptics = {
      enable = true;
      tapButtons = false;
      vertTwoFingerScroll = true;
      horizTwoFingerScroll = true;
      maxSpeed = "2.0";
    };

    autoRepeatInterval = 50;
    autoRepeatDelay = 200;
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
    extraGroups = [ "wheel" "networkmanager" ];
  };

  environment.systemPackages = with pkgs; [
    vim
    efibootmgr
    refind
    wget
    pciutils
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
  system.stateVersion = "21.05"; # Did you read the comment?

}

