import Xmobar

import Helpers
import qualified Theme

config :: Config
config = defaultConfig { 

   -- appearance
     --font            = font'
     font            = "xft:Hack:size=8:bold:antialias=true"
   , bgColor         = Theme.background
   , additionalFonts = additionalFonts'
   , fgColor         = Theme.foreground
   , alpha           = 255
   , position        = OnScreen 0 $ Top
--    , position    =  Static {xpos = 0, ypos = 0, width = 1920, height = 15}
   , borderColor     = "#646464"

   -- layout
   , sepChar =  "%"   -- delineator between plugin names and straight text
   , alignSep = "}{"  -- separator between left-right alignment
   , template = (setColor Theme.color1 "TEST") ++ " }{ <fc=#ff0000,#00ff00>TEST</fc><fn=0><fc=#00ff00,#ff0000>\xe0b0</fc></fn>"

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
