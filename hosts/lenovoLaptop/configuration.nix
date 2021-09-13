{ config, pkgs, secrets, wallpapers, ... }:

{
  imports =
    [
      ./hardware-configuration.nix
    ];

  boot.supportedFilesystems = [ "ntfs" ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  time.timeZone = "Europe/Moscow";

  hardware.pulseaudio = {
    enable = true;
    package = pkgs.pulseaudioFull;
  };
  programs.dconf.enable = true;

  networking = {
    hostName = "NixLenovo";
    # useDHCP = false;
    interfaces = {
      enp2s0.useDHCP = true;
      wlp3s0.useDHCP = true;
    }; 
    networkmanager = {
      enable = true;
    };

    firewall = {
      checkReversePath = false;
    };
  };

  i18n = {
    defaultLocale = "en_US.UTF-8";
    supportedLocales = [
      "en_US.UTF-8/UTF-8"
      "ru_RU.UTF-8/UTF-8"
    ];
  };

  programs.steam.enable = true;

  services.udev.extraRules = ''
    ACTION=="add", SUBSYSTEM=="backlight", KERNEL=="intel_backlight", MODE="0666", RUN+="${pkgs.coreutils}/bin/chmod a+w /sys/class/backlight/%k/brightness"
  '';

  services.xserver = {
    enable = true;

    windowManager.xmonad.enable = true;

    displayManager.defaultSession = "none+xmonad";
    displayManager.lightdm.greeters.mini = {
      enable = true;
      user = "iliayar";
      extraConfig = ''
                [greeter]
                show-password-label = false
                password-alignment = left
                [greeter-theme]
                background-image = "${wallpapers}/HtmoocM.jpg"
      '';
    };

    displayManager.sessionCommands = ''
      ${pkgs.xlibs.xsetroot}/bin/xsetroot -xcf ${pkgs.vanilla-dmz}/share/icons/Vanilla-DMZ/cursors/left_ptr 16
    '';

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

  services.tlp = {
    enable = true;
  };

  services.blueman.enable = true;
  hardware.bluetooth.enable = true;

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

