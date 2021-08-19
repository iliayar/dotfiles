module Theme where

import qualified GHC.Word

color0     = "{color0}"
color1     = "{color1}"
color2     = "{color2}"
color3     = "{color3}"
color4     = "{color4}"
color5     = "{color5}"
color6     = "{color6}"
color7     = "{color7}"
color8     = "{color8}"
color9     = "{color9}"
color10    = "{color10}"
color11    = "{color11}"
color12    = "{color12}"
color13    = "{color13}"
color14    = "{color14}"
color15    = "{color15}"
background = "{background}"
foreground = "{foreground}"
cursor     = "{cursor}"

ts_background, ts_extra :: GHC.Word.Word64
ts_node, ts_nodealt, ts_highlight :: (GHC.Word.Word64, GHC.Word.Word64)
ts_background   = 0x00{color0.strip}
ts_node         = (0xff{color15.strip}, 0xff{background.strip})
ts_nodealt      = (0xff{color15.strip}, 0xff{color0.strip})
ts_highlight    = (0xff{color15.strip}, 0xff{color8.strip})
ts_extra        = 0xff{color7.strip}

