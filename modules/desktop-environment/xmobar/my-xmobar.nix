{ mkDerivation, base, lib, xmobar }:
mkDerivation {
  pname = "my-xmobar";
  version = "1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base xmobar ];
  license = lib.licenses.bsd3;
}
