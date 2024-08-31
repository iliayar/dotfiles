{ config, pkgs, secrets, wallpapers, ... }:

let
  nvidia-offload = pkgs.writeShellScriptBin "nvidia-offload" ''
    export __NV_PRIME_RENDER_OFFLOAD=1
    export __NV_PRIME_RENDER_OFFLOAD_PROVIDER=NVIDIA-G0
    export __GLX_VENDOR_LIBRARY_NAME=nvidia
    export __VK_LAYER_NV_optimus=NVIDIA_only
    exec -a "$0" "$@"
  '';
in {
  imports = [ ./hardware-configuration.nix ../laptop.nix ];

  boot.supportedFilesystems = [ "ntfs" ];

  boot.loader.grub.enable = true;
  boot.loader.grub.device = "nodev";
  boot.loader.grub.efiSupport = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.efi.efiSysMountPoint = "/boot/";

  # boot.kernelPackages = pkgs.linuxPackages_5_15;
  boot.extraModulePackages = with config.boot.kernelPackages; [ nvidia_x11 v4l2loopback  ];
  boot.kernelModules = [ "v4l2loopback" ];

  time.timeZone = "Europe/Moscow";

  security.polkit.enable = true;
  security.rtkit.enable = true;

  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    wireplumber.enable = true;
    jack.enable = true;
  };
  programs.dconf.enable = true;

  networking = {
    hostName = "NixLaptop";
    interfaces = { enp60s0.useDHCP = true; };
    networkmanager = { enable = true; };
    firewall.checkReversePath = false;
  };

  i18n = {
    defaultLocale = "en_US.UTF-8";
    supportedLocales = [ "en_US.UTF-8/UTF-8" "ru_RU.UTF-8/UTF-8" ];
  };

  programs.steam.enable = true;

  services.udev.extraRules = ''
    ACTION=="add", SUBSYSTEM=="backlight", KERNEL=="intel_backlight", MODE="0666", RUN+="${pkgs.coreutils}/bin/chmod a+w /sys/class/backlight/%k/brightness"
  '';

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


  services.displayManager.sddm.enable = false;
  services.xserver = {
    enable = false;
    desktopManager.plasma5.enable = false;

    dpi = 96;

    autoRepeatInterval = 50;
    autoRepeatDelay = 200;

    xkb.options = "grp:switch,grp:caps_toggle,altwin:swap_alt_win";
    xkb.layout = "us,ru";
  };

  # services.xserver = {
  #   enable = true;
  #   videoDrivers = [ "nvidia" ];
  #   screenSection = ''
  #     Option         "metamodes" "nvidia-auto-select +0+0 {ForceFullCompositionPipeline=On}"
  #     Option         "AllowIndirectGLXProtocol" "off"
  #     Option         "TripleBuffer" "on"
  #   '';
  #
  #   displayManager.sddm.enable = false;
  #   desktopManager.plasma5.enable = false;
  #
  #   # Conflicts with pipewire
  #   desktopManager.gnome.enable = false;
  #
  #   dpi = 96;
  #   # windowManager.xmonad.enable = true;
  #
  #   # displayManager.defaultSession = "none+xmonad";
  #   # displayManager.lightdm.greeters.mini = {
  #   #   enable = true;
  #   #   user = "iliayar";
  #   #   extraConfig = ''
  #   #             [greeter]
  #   #             show-password-label = false
  #   #             password-alignment = left
  #   #             [greeter-theme]
  #   #             background-image = "${wallpapers}/HtmoocM.jpg"
  #   #   '';
  #   # };
  #   #  
  #   # displayManager.sessionCommands = ''
  #   #   ${pkgs.xorg.xsetroot}/bin/xsetroot -xcf ${pkgs.vanilla-dmz}/share/icons/Vanilla-DMZ/cursors/left_ptr 16
  #   # '';
  #
  #   libinput.enable = false;
  #   synaptics = {
  #     enable = true;
  #     tapButtons = false;
  #     vertTwoFingerScroll = true;
  #     horizTwoFingerScroll = true;
  #     minSpeed = "0.6";
  #     maxSpeed = "2.4";
  #   };
  #
  #   autoRepeatInterval = 50;
  #   autoRepeatDelay = 200;
  #
  #   xkbOptions = "grp:switch,grp:caps_toggle,altwin:swap_alt_win";
  #   layout = "us,ru";
  # }; 

  services.blueman.enable = true;
  hardware.bluetooth.enable = true;

  # It's also valid for wayland
  services.xserver.videoDrivers = [ "nvidia" ];

  hardware = {
    graphics = {
      enable = true;
    };
    nvidia = {
      prime = {
        sync.enable = true;
        intelBusId = "PCI:0:2:0";
        nvidiaBusId = "PCI:1:0:0";
      };
      modesetting.enable = true;
      package = config.boot.kernelPackages.nvidiaPackages.stable;
      open = false;
    };

    keyboard.qmk.enable = true;
  };

  # hardware.pulseaudio.enable = false;

  nix = {
    package = pkgs.nixFlakes;
    settings = { trusted-users = [ "root" "iliayar" ]; };
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };

  users.users.iliayar = {
    isNormalUser = true;
    home = "/home/iliayar";
    extraGroups = [ "wheel" "networkmanager" "video" "libvirtd" ];
    shell = pkgs.zsh;
  };
  programs.zsh.enable = true;

  environment.variables = {
    # GDK_SCALE = "1";
  };

  virtualisation.libvirtd.enable = true;

  # virtualisation.virtualbox.host.enable = true;
  # users.extraGroups.vboxusers.members = [ "iliayar" ];
  # virtualisation.virtualbox.host.enableExtensionPack = true;

  environment.systemPackages = with pkgs; [
    vim
    efibootmgr
    refind
    wget
    pciutils
    nvidia-offload
    virt-manager
    cachix

    # config.boot.kernelPackages.vm-tools
    config.boot.kernelPackages.perf

    xdg-utils
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
  system.stateVersion = "22.05"; # Did you read the comment?

}
