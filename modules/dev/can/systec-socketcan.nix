{ pkgs, systec-can, ... }:
let 
  stdenv = pkgs.stdenv;
  kernel = pkgs.linuxPackages.kernel;
in
stdenv.mkDerivation {
  name = "systec-socketcan";

  src = systec-can;

  nativeBuildInputs = kernel.moduleBuildDependencies ++ [ pkgs.kmod ];

  buildDepends = with pkgs; [
    pkgs.kmod
  ];

  preInstallPhases = [ "fixDepmod" ];
  preDistPhases = [ "installModule" ];

  makeFlags = [
    "KDIR=${kernel.dev}/lib/modules/${kernel.modDirVersion}/build"
    "INSTALL_MOD_PATH=$(out)"
    "FW_DIR=$(out)/lib/firmware"
  ];

  fixDepmod = ''
    sed -ie "s@depmod@depmod -b ${kernel.dev}@" Makefile
  '';

  installModule = ''
    cp systec_can.ko $out/lib/modules/${kernel.modDirVersion}
  '';

  shellHook = ''
    echo sysWORXX Sockercan driver
  '';
}
