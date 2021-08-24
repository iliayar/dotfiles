{ pkgs, ... }:
pkgs.rustPlatform.buildRustPackage {
  name = "rust-blocks";
  src = ./.;
  buildInputs = [ pkgs.dbus ];
  nativeBuildInputs = [ pkgs.pkgconfig ];
  cargoLock = {
    lockFile = ./Cargo.lock;
  };
}
