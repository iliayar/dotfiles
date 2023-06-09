{ mylib, ... }:
with builtins;
let
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

  createMap = f: mapAttrs (_: c: f (mylib.strToColor c));
  defMap = s: f: th: { "${s}" = createMap f th; };
  withMaps = maps: th: foldl' (a: v: a // v) th (map (m: m th) maps);
  tsColor = alpha: c: "0x${mylib.decToHex2 alpha}${mylib.decToHex2 c.r}${mylib.decToHex2 c.g}${mylib.decToHex2 c.b}";
  rgba = c: alpha: "rgba(${toString c.r}, ${toString c.g}, ${toString c.b}, ${toString alpha})";
in
withMaps [
  (defMap "ts" (tsColor 255))
  (defMap "tsTransparent" (tsColor 0))
  (defMap "rgba" rgba)
] theme
