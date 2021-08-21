{
  pkgs ? import <nixpkgs> { }
}:
let 
  stdenv = pkgs.stdenv;
  kernel = pkgs.linuxPackages.kernel;
in
stdenv.mkDerivation {
  name = "systec-socketcan";

  src = pkgs.fetchurl {
    url = "https://www.systec-electronic.com/fileadmin/Redakteur/Unternehmen/Support/Downloadbereich/Treiber/systec_can-V1.0.3.tar.bz2";
    sha256 = "PIohvsMh8WOayZ4NutXCXTCUw/XLB0/FUBgg8T1naag=";
  };

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
