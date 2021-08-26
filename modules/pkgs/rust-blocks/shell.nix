{
  pkgs ? import <nixpkgs> { }
}:
pkgs.mkShell {
  buildInputs = [ pkgs.dbus ];
  nativeBuildInputs = with pkgs; [ cargo rustc rustfmt pkgconfig rust-analyzer ];
}
