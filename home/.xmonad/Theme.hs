module Theme where

import qualified GHC.Word

color0     = "#272822"
color1     = "#f92672"
color2     = "#a6e22e"
color3     = "#f4bf75"
color4     = "#66d9ef"
color5     = "#ae81ff"
color6     = "#a1efe4"
color7     = "#f8f8f2"
color8     = "#75715e"
color9     = "#f92672"
color10    = "#a6e22e"
color11    = "#f4bf75"
color12    = "#66d9ef"
color13    = "#ae81ff"
color14    = "#a1efe4"
color15    = "#f9f8f5"
background = "#1c1e1f"
foreground = "#f8f8f2"
cursor     = "#f92672"

ts_background, ts_extra :: GHC.Word.Word64
ts_node, ts_nodealt, ts_highlight :: (GHC.Word.Word64, GHC.Word.Word64)
ts_background   = 0x00272822
ts_node         = (0xfff9f8f5, 0xff1c1e1f)
ts_nodealt      = (0xfff9f8f5, 0xff272822)
ts_highlight    = (0xfff9f8f5, 0xff75715e)
ts_extra        = 0xfff8f8f2

