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
   , alpha           = 0
   , position        = OnScreen 0 $ Bottom
--    , position    =  Static {xpos = 0, ypos = 1060, width = 1364, height = 20}
   , borderColor     = "#646464"

   -- layout
   , sepChar  = "%"   -- delineator between plugin names and straight text
   , alignSep = "}{"  -- separator between left-right alignment
   , template = "<action=~/.xmonad/xmonadctl 1>" ++ (icon "nix") ++ "</action> | %UnsafeStdinReader% }{ %disku% | %bright% | %sound% | %kbd% | %memory% | %multicpu% | %dynnetwork% | %battery% | %multicoretemp% | %date% "

   -- general behavior
   , lowerOnStart     = True    -- send to bottom of window stack on start
   , hideOnStart      = False   -- start with window unmapped (hidden)
   , allDesktops      = True    -- show on all desktops
   , overrideRedirect = True    -- set the Override Theme.Color1irect flag (Xlib)
   , pickBroadest     = False   -- choose widest display (multi-monitor)
   , persistent       = True    -- enable/disable hiding (True = disabled)
   , iconRoot         = "/home/iliayar/.xmonad/xpm/"  -- default: "."

   , commands = 

        [ Run $ Weather "RJTT" [ "--template", "<station> | <skyCondition> | <fc=#4682B4><tempC></fc>°C | <fc=#4682B4><rh></fc>% | <fc=#4682B4><pressure></fc>hPa"
                             ] 36000

        -- Screen brightness
        , Run $ Brightness   [ "--template" , (afIcon "\xf185") ++ " <percent>"
                             , "--"
                             , "-D" , "intel_backlight"
                             ] 10

        -- Volume
        , Run $ PipeReader "/tmp/sound.io" "sound"
        -- , Run $ Volume "default" "Master"
        --                      [ "--template" , "<action=pactl set-sink-mute '@DEFAULT_SINK@' toggle><status> <volume>%</action>"
        --                      , "--"
        --                      , "--on"       , afIcon "\xf028"
        --                      , "--onc"      , Theme.color2
        --                      , "--off"      , afIcon "\xf6a9"
        --                      , "--offc"     , Theme.color1
        --                      ] 10

        -- network activity monitor (dynamic interface resolution)
        , Run $ DynNetwork   [ "--template" , "<dev>: " ++ (afIcon "\xf063") ++ "<rx>kB/s " ++ (afIcon "\xf062") ++ "<tx>kB/s"
                             , "--width"    , "4"
                             , "--Low"      , "1000000"       -- units: B/s
                             , "--High"     , "5000000"       -- units: B/s
                             , "--low"      , Theme.color2
                             , "--normal"   , Theme.color3
                             , "--high"     , Theme.color1
                             ] 10

        -- cpu activity monitor
        , Run $ MultiCpu     [ "--template" , (afIcon "\xf2db") ++ " <total0>%|<total1>%"
                             , "--width"    , "2"
                             , "--Low"      , "50"         -- units: %
                             , "--High"     , "85"         -- units: %
                             , "--low"      , Theme.color2
                             , "--normal"   , Theme.color3
                             , "--high"     , Theme.color1
                             ] 10

        -- cpu core temperature monitor
        , Run $ MultiCoreTemp       [ "--template" , (afIcon "\xf2c9") ++ " <avg>°C"
                             , "--Low"      , "70"        -- units: °C
                             , "--High"     , "80"        -- units: °C
                             , "--low"      , Theme.color2
                             , "--normal"   , Theme.color3
                             , "--high"     , Theme.color1
                             ] 50
                          
        -- memory usage monitor
        , Run $ Memory       [ "--template" , (afIcon "\xf538") ++ " <available> M"
                             , "--Low"      , "2000"        -- units: M
                             , "--High"     , "6000"        -- units: M
                             , "--low"      , Theme.color1
                             , "--normal"   , Theme.color3
                             , "--high"     , Theme.color2
                             ] 10

        -- battery monitor
        , Run $ Battery        [ "--template" ,  "<acstatus>"
                               , "--Low"      , "20"        -- units: %
                               , "--High"     , "70"        -- units: %
                               , "--low"      , Theme.color1
                               , "--normal"   , Theme.color3
                               , "--high"     , Theme.color2
                               , "--" -- battery specific options
                                       -- discharging status
                                       , "-o"    , "<left>% <watts>"
                                       -- AC "on" status
                                       , "-O"    , afIcon "\xf242"
                                       -- charged status
                                       , "-i"    , afIcon "\xf240"
                                       , "-L", "-15"
                                       , "-H", "-5"
                                       , "-l", Theme.color1
                                       , "-m", Theme.color3
                                       , "-h", Theme.color2
                                       , "-p", Theme.color2
                                       , "--lows", afIcon "\xf244 "
                                       , "--mediums", afIcon "\xf242 "
                                       , "--highs", afIcon "\xf240 "
                             ] 50

        -- time and date indicator 
        --   (%F = y-m-d date, %a = day of week, %T = h:m:s time)
        , Run $ Date           (afIcon "\xf783" ++ " %F (%a) %T") "date" 10

        -- -- keyboard layout indicator
        , Run $ Kbd          [ ("ru"         , setColor Theme.color1 "RU")
                             , ("us"         , setColor Theme.color2 "US")
                             ]
        , Run $ DiskU [("/", "/ <usedp>%")]
                    [ "--Low"      , "30"
                    , "--High"     , "60"
                    , "--low"      , Theme.color2
                    , "--normal"   , Theme.color3
                    , "--high"     , Theme.color1
                    ]
                    20

        -- Xmonad worspaces and program title
        , Run $ UnsafeStdinReader
        -- , Run $ Com "bash" ["-c", "~/bin/blocks/pacman_xmobar.sh &"] "updates" 36000
        -- , Run $ Com "bash" ["-c", "~/bin/blocks/music_xmobar.sh &"] "music" 50
        ]
   }
main :: IO ()
main = xmobar $ config
