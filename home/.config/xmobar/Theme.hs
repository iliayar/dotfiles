module Theme where

import qualified GHC.Word

color0     = "#1d2021"
color1     = "#fb4934"
color2     = "#b8bb26"
color3     = "#fabd2f"
color4     = "#83a598"
color5     = "#d3869b"
color6     = "#8ec07c"
color7     = "#d5c4a1"
color8     = "#665c54"
color9     = "#fb4934"
color10    = "#b8bb26"
color11    = "#fabd2f"
color12    = "#83a598"
color13    = "#d3869b"
color14    = "#8ec07c"
color15    = "#fbf1c7"
background = "#1d2021"
foreground = "#d5c4a1"
cursor     = "#d5c4a1"

ts_background, ts_extra :: GHC.Word.Word64
ts_node, ts_nodealt, ts_highlight :: (GHC.Word.Word64, GHC.Word.Word64)
ts_background   = 0x001d2021
ts_node         = (0xfffbf1c7, 0xff1d2021)
ts_nodealt      = (0xfffbf1c7, 0xff1d2021)
ts_highlight    = (0xfffbf1c7, 0xff665c54)
ts_extra        = 0xffd5c4a1

