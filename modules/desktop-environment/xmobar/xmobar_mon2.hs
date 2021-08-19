import Xmobar

import Helpers
import qualified Theme

config :: Config
config = defaultConfig { 

   -- appearance
     font            = font'
   , bgColor         = Theme.background
   , additionalFonts = additionalFonts'
   , fgColor         = Theme.foreground
   , alpha           = 255
   , position        = OnScreen 1 $ Bottom
--    , position    =  Static {xpos = 0, ypos = 1060, width = 1364, height = 20}
   , borderColor     = "#646464"

   -- layout
   , sepChar =  "%"   -- delineator between plugin names and straight text
   , alignSep = "}{"  -- separator between left-right alignment
   , template = "<action=~/.xmonad/xmonadctl 1><icon=edved.xpm/></action> | %UnsafeStdinReader% }{ %date% "

   -- general behavior
   , lowerOnStart     = True    -- send to bottom of window stack on start
   , hideOnStart      = False   -- start with window unmapped (hidden)
   , allDesktops      = True    -- show on all desktops
   , overrideRedirect = True    -- set the Override Redirect flag (Xlib)
   , pickBroadest     = False   -- choose widest display (multi-monitor)
   , persistent       = True    -- enable/disable hiding (True = disabled)
   , iconRoot         = "/home/iliayar/.xmonad/xpm/"  -- default: "."

   , commands = 

        [  Run $ Date           "%F (%a) %T" "date" 10

        -- Xmonad worspaces and program title
        , Run $ UnsafeStdinReader
        ]
   }
main :: IO ()
main = xmobar $ config
