{ pkgs, ... }:
pkgs.stdenv.mkDerivation rec {
  pname = "rz-ghidra";
  version = "0.2.0";

  src = pkgs.fetchFromGitHub {
    owner = "rizinorg";
    repo = pname;
    rev = "v${version}";
    sha256 = "sha256-SnRNOv2rGGUX+/0zHIPptpt+t7c9FGJ9mFQT6Y16IxM=";
    fetchSubmodules = true;
  };

  nativeBuildInputs = with pkgs; [
    cmake
    pkg-config
    libsForQt5.wrapQtAppsHook
  ];

  buildInputs = with pkgs; [
    cutter
    rizin
  ];

  cmakeFlags = [
    "-DBUILD_CUTTER_PLUGIN=ON"
  ];

  postPhases = [ "fixLibraries" ];

  fixLibraries = ''
               file=$out/share/rizin/cutter/plugins/native/librz_ghidra_cutter.so
               patchelf --set-rpath $(patchelf --print-rpath $file):$out/share/rizin/plugins $file
  '';
}
