{ pkgs }:
with builtins;
rec {
  matchColor = match "#([0-9a-fA-F]{2})([0-9a-fA-F]{2})([0-9a-fA-F]{2})";
  hexToDec = {
    "0" = 0; "1" = 1; "2" = 2; "3" = 3;
    "4" = 4; "5" = 5; "6" = 6; "7" = 7;
    "8" = 8; "9" = 9; "a" = 10; "b" = 11;
    "c" = 12; "d" = 13; "e" = 14; "f" = 15;
  };
  hex2ToDec = s:
    let
      l = pkgs.lib.stringToCharacters s;
    in
      hexToDec."${(elemAt l 0)}" * 16 + hexToDec."${elemAt l 1}";
  strToColor = s:
    let
      ns = pkgs.lib.toLower s;
      l = matchColor ns;
    in mapAttrs (_: s: hex2ToDec s) {
      r = elemAt l 0;
      g = elemAt l 1;
      b = elemAt l 2;
    };
  decToHex = [ "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B" "C" "D" "E" "F" ];
  mod = base: int: base - (int * (builtins.div base int));
  decToHex2 = n:
    let
      hi = builtins.div n 16;
      lo = mod n 16;
    in "${elemAt decToHex hi}${elemAt decToHex lo}";

  writePython3Bin = name: deps: text:
    let
      python = pkgs.python39.withPackages deps;
    in
      pkgs.writeTextFile {
        name = name;
        executable = true;
        destination = "/bin/${name}";
        text = ''
      #!${python}/bin/python
      ${text}
      '';
      };
}
