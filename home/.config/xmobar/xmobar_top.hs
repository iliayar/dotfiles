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
   , position        = OnScreen 0 $ Top
--    , position    =  Static {xpos = 0, ypos = 1060, width = 1364, height = 20}
   , borderColor     = "#646464"

   -- layout
   , sepChar =  "%"   -- delineator between plugin names and straight text
   , alignSep = "}{"  -- separator between left-right alignment
   , template = ""

   -- general behavior
   , lowerOnStart     = True    -- send to bottom of window stack on start
   , hideOnStart      = False   -- start with window unmapped (hidden)
   , allDesktops      = True    -- show on all desktops
   , overrideRedirect = True    -- set the Override Redirect flag (Xlib)
   , pickBroadest     = False   -- choose widest display (multi-monitor)
   , persistent       = True    -- enable/disable hiding (True = disabled)
   , iconRoot         = "/home/iliayar/.xmonad/xpm/"  -- default: "."

   , commands = 

        [
        ]
   }
main :: IO ()
main = xmobar $ config
