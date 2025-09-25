{ config, lib, pkgs, ... }:

{
  imports =
    [
      ./hardware-configuration.nix
    ];

  boot.extraModulePackages = [
    # pkgs.systec-can
  ];
  hardware.firmware = [
    # pkgs.systec-can
  ];


  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.supportedFilesystems = [ "ntfs" ];

  boot.binfmt.emulatedSystems = [ "aarch64-linux" ];

  boot = {
    plymouth = {
      enable = true;
      theme = "rings";
      themePackages = with pkgs; [
        (adi1090x-plymouth-themes.override {
          selected_themes = [ "rings" ];
        })
      ];
    };

    consoleLogLevel = 0;
    initrd.verbose = false;
    initrd.systemd.enable = true;
    kernelParams = [
      "quiet"
      "splash"
      "boot.shell_on_fail"
      "loglevel=3"
      "rd.systemd.show_status=false"
      "rd.udev.log_level=3"
      "udev.log_priority=3"
    ];
  };

  networking = {
    hostName = "NixPC";
    networkmanager = { enable = true; };
    nameservers = [ "1.1.1.1" "8.8.8.8" "8.8.4.4" ];

    firewall = {
        enable = false;
    };
  };

  networking.interfaces."enp10s0".wakeOnLan = {
    enable = true;
    policy = [ "magic" ];
  };

  time.timeZone = "Europe/Moscow";

  i18n = {
    defaultLocale = "en_US.UTF-8";
    supportedLocales = [ "en_US.UTF-8/UTF-8" "ru_RU.UTF-8/UTF-8" ];
  };

  security.polkit.enable = true;
  security.rtkit.enable = true;

  services.pipewire = {
    enable = true;
    pulse.enable = true;
    wireplumber.enable = true;
  };

  users.users.iliayar = {
    isNormalUser = true;
    home = "/home/iliayar";
    extraGroups = [ "wheel" "networkmanager" "video" "libvirtd" "wireshark" "dialout" "docker" "audio" ];
    shell = pkgs.zsh;
  };
  programs.zsh.enable = true;

  security.sudo = {
    enable = true;
    extraConfig = ''
      Defaults pwfeedback
    '';
  };

  environment.systemPackages = with pkgs; [
    vim
    git

    # GPU stuff
    clinfo
  ];

  nix = {
    settings = {
      trusted-users = [ "iliayar" ];
      secret-key-files = [ "/var/iliayar-cache-priv-key.pem" ];
    };
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };

  hardware = {
    graphics = { 
        enable = true;
        extraPackages = with pkgs; [ rocmPackages.clr.icd ];
    };
    keyboard.qmk.enable = true;
  };

  services.xserver.videoDrivers = [ "amdgpu" ];

  hardware.bluetooth.enable = true;
  services.blueman.enable = true;

  services.greetd = {
    enable = true;
    settings = {
      default_session = {
        command = "${pkgs.tuigreet}/bin/tuigreet -t -r -c Hyprland";
        user = "greeter";
      };
    };
  };

  programs.dconf.enable = true;
  programs.steam.enable = true;
  programs.nix-ld.enable = true;

  services.resolved.enable = true;

  services.hardware.openrgb.enable = true;

  # FIXME: MX Master 3s keeps wakes up system after suspend
  # Not persistent workaround: echo "XHC2" | sudo tee /proc/acpi/wakeup
  #
  # $ grep XHC2 /proc/acpi/wakeup
  # XHC2      S4    *disabled  pci:0000:13:00.0
  # $ cat /sys/class/pci_bus/0000:13/device/vendor
  # 0x1022
  # $ cat /sys/class/pci_bus/0000:13/device/device
  # 0x14dd
  services.udev.extraRules = ''
    ACTION=="add" SUBSYSTEM=="pci" ATTR{vendor}=="0x1022" ATTR{device}=="0x14dd" ATTR{power/wakeup}="disabled"
  '';

  virtualisation.libvirtd.enable = true;
  virtualisation.virtualbox.host.enable = true;
  virtualisation.virtualbox.host.enableExtensionPack = true;
  users.extraGroups.vboxusers.members = [ "iliayar" ];

  virtualisation.docker = {
    enable = true;
    extraPackages = with pkgs; [
        docker-compose
    ];
  };

  programs.wireshark.enable = true;

  security.pam.services.kwallet = {
    kwallet.enable = true;
  };

  services.openssh = {
    enable = true;
  };

  users.users."iliayar".openssh.authorizedKeys.keys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIQS/EGCyrNBP1GKIm+BSHMIrwC+9TK9C/HyXAPDHLKK"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIEm3biV/gVmzzgwH/K8lV/fM7Noa9lmWlJSXpEmWyE9s"
  ];
  programs.mosh.enable = true;

  services.gns3-server = {
    enable = true;
    ubridge.enable = true;
    vpcs.enable = true;
    dynamips.enable = true;
  };

  services.avahi = {
    enable = true;
    nssmdns4 = true;  # printing
    publish = {
      enable = true;
      addresses = true;
      workstation = true;
      userServices = true;
    };
  };

  services.udisks2.enable = true;

  # This option defines the first version of NixOS you have installed on this particular machine,
  # and is used to maintain compatibility with application data (e.g. databases) created on older NixOS versions.
  #
  # Most users should NEVER change this value after the initial install, for any reason,
  # even if you've upgraded your system to a new NixOS release.
  #
  # This value does NOT affect the Nixpkgs version your packages and OS are pulled from,
  # so changing it will NOT upgrade your system - see https://nixos.org/manual/nixos/stable/#sec-upgrading for how
  # to actually do that.
  #
  # This value being lower than the current NixOS release does NOT mean your system is
  # out of date, out of support, or vulnerable.
  #
  # Do NOT change this value unless you have manually inspected all the changes it would make to your configuration,
  # and migrated your data accordingly.
  #
  # For more information, see `man configuration.nix` or https://nixos.org/manual/nixos/stable/options#opt-system.stateVersion .
  system.stateVersion = "24.11"; # Did you read the comment?

}
