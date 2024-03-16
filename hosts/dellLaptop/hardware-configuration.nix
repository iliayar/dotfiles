# Do not modify this file!  It was generated by ‘nixos-generate-config’
# and may be overwritten by future invocations.  Please make changes
# to /etc/nixos/configuration.nix instead.
{ config, lib, pkgs, modulesPath, ... }:

{
  imports =
    [ (modulesPath + "/installer/scan/not-detected.nix")
    ];

  boot.initrd.availableKernelModules = [ "xhci_pci" "ahci" "nvme" "usb_storage" "sd_mod" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.kernelParams = [ "quite" ];
  boot.extraModulePackages = [ 
    # pkgs.systec-can 
  ];
  hardware.firmware = [ 
    # pkgs.systec-can 
  ];

  fileSystems."/" =
    { device = "/dev/disk/by-uuid/a2efc398-ec8d-4ba4-a599-fbdff9b17e03";
      fsType = "ext4";
      neededForBoot = true;
    };

  boot.initrd.luks.devices."cryptroot".device = "/dev/disk/by-uuid/f14dc83e-edb5-4e2c-8ec5-b676366c914d";

  fileSystems."/boot" =
    { device = "/dev/disk/by-uuid/F239-4E11";
      fsType = "vfat";
    };

  fileSystems."/home" =
    { device = "/dev/disk/by-uuid/4830a5d3-9e69-46d2-bcba-6f7d8f67803a";
      encrypted = {
        enable = true;
        label = "crypthome";
        blkDev = "/dev/disk/by-uuid/25c60fb2-cffe-499a-87c1-6a0c67fcbee4";
        keyFile = "/mnt-root/etc/keyfiles/crypthome";
      };
      fsType = "ext4";
    };

  swapDevices = [ ];

  powerManagement.cpuFreqGovernor = lib.mkDefault "performance";
  hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
}
