{ config, lib, pkgs, ... }:

{
  imports =
    [
      ./hardware-configuration.nix
    ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking = {
    hostName = "NixPC";
    networkmanager = { enable = true; };
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
  ];

  nix = {
    settings = {
      trusted-users = [ "iliayar" ];
      # TODO: Restore
      # secret-key-files = [ "/var/iliayar-cache-priv-key.pem" ];
    };
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };

  hardware = {
    graphics = { enable = true; };
    keyboard.qmk.enable = true;
  };

  hardware.bluetooth.enable = true;
  services.blueman.enable = true;

  services.greetd = {
    enable = true;
    settings = {
      default_session = {
        command = "${pkgs.greetd.tuigreet}/bin/tuigreet -t -r -c Hyprland";
        user = "greeter";
      };
    };
    vt = 6;
  };

  programs.dconf.enable = true;
  programs.steam.enable = true;

  services.resolved.enable = true;

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

