module Theme where

import qualified GHC.Word

color0     = "#002b36"
color1     = "#dc322f"
color2     = "#859900"
color3     = "#b58900"
color4     = "#268bd2"
color5     = "#6c71c4"
color6     = "#2aa198"
color7     = "#93a1a1"
color8     = "#657b83"
color9     = "#dc322f"
color10    = "#859900"
color11    = "#b58900"
color12    = "#268bd2"
color13    = "#6c71c4"
color14    = "#2aa198"
color15    = "#fdf6e3"
background = "#002b36"
foreground = "#93a1a1"
cursor     = "#93a1a1"

ts_background, ts_extra :: GHC.Word.Word64
ts_node, ts_nodealt, ts_highlight :: (GHC.Word.Word64, GHC.Word.Word64)
ts_background   = 0x00002b36
ts_node         = (0xfffdf6e3, 0xff002b36)
ts_nodealt      = (0xfffdf6e3, 0xff002b36)
ts_highlight    = (0xfffdf6e3, 0xff657b83)
ts_extra        = 0xff93a1a1

