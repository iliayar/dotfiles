{ lib, ... }:
with builtins;
let
  matchColor = match "#([0-9a-fA-F]{2})([0-9a-fA-F]{2})([0-9a-fA-F]{2})";
  hexToDec = {
    "0" = 0; "1" = 1; "2" = 2; "3" = 3;
    "4" = 4; "5" = 5; "6" = 6; "7" = 7;
    "8" = 8; "9" = 9; "a" = 10; "b" = 11;
    "c" = 12; "d" = 13; "e" = 14; "f" = 15;
  };
  hex2ToDec = s:
    let
      l = lib.stringToCharacters s;
    in
      hexToDec."${(elemAt l 0)}" * 16 + hexToDec."${elemAt l 1}";
  strToColor = s:
    let
      s = lib.toLower s;
      l = matchColor s;
    in mapAttrs (_: s: hex2ToDec s) {
      r = elemAt l 0;
      g = elemAt l 1;
      b = elemAt l 2;
    };

  theme = rec {
    cursor        = "#f92672";
    cursorText    = "#272822";
    background    = "#1c1e1f";
    foreground    = "#f8f8f2";
    black         = "#272822";
    red           = "#e3276b";
    yellow        = "#f4bf75";
    green         = "#a6e22e";
    blue          = "#66d9ef";
    magenta       = "#ae81ff";
    cyan          = "#a1efe4";
    white         = "#f8f8f2";
    brightBlack   = "#75715e";
    brightRed     = "#e3276b";
    brightYellow  = "#f4bf75";
    brightGreen   = "#a6e22e";
    brightBlue    = "#66d9ef";
    brightMagenta = "#ae81ff";
    brightCyan    = "#a1efe4";
    brightWhite   = "#f9f8f5";

    color0  = black;
    color1  = red;
    color2  = green;
    color3  = yellow;
    color4  = blue;
    color5  = magenta;
    color6  = cyan;
    color7  = white;
    color8  = brightBlack;
    color9  = brightRed;
    color10 = brightGreen;
    color11 = brightYellow;
    color12 = brightBlue;
    color13 = brightMagenta;
    color14 = brightCyan;
    color15 = brightWhite;

    font = "Fira Code";
  };

  createMap = f: mapAttrs (_: c: f (strToColor c));
  defMap = s: f: th: { "${s}" = createMap f th; };
  withMaps = maps: th: foldl' (a: v: a // v) th (map (m: m th) maps);
in
withMaps [

] theme
