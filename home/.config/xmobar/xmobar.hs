import Xmobar

import Helpers

config :: Config
config = defaultConfig { 

   -- appearance
     font =         "xft:Hack:size=8:bold:antialias=true"
   , bgColor     =  "#1d2021"
   , additionalFonts = ["xft:FontAwesome:pixelsize=12"]
   , fgColor     =  "#ebdbb2"
   , alpha       =  255
   , position   = OnScreen 0 $ Bottom
--    , position    =  Static {xpos = 0, ypos = 1060, width = 1364, height = 20}
   , borderColor =  "#646464"

   -- layout
   , sepChar =  "%"   -- delineator between plugin names and straight text
   , alignSep = "}{"  -- separator between left-right alignment
   , template = "<action=~/.xmonad/xmonadctl 1>" ++ (icon "edved") ++ "</action> | %UnsafeStdinReader% }{ %music% | %updates% | %disku% | %bright% | %default:Master% | %kbd% | %memory% | %multicpu% | %dynnetwork% | %battery% | %multicoretemp% | %date% "

   -- general behavior
   , lowerOnStart =     True    -- send to bottom of window stack on start
   , hideOnStart =      False   -- start with window unmapped (hidden)
   , allDesktops =      True    -- show on all desktops
   , overrideRedirect = True    -- set the Override Redirect flag (Xlib)
   , pickBroadest =     False   -- choose widest display (multi-monitor)
   , persistent =       True    -- enable/disable hiding (True = disabled)
   , iconRoot = "/home/iliayar/.xmonad/xpm/"  -- default: "."

   , commands = 

        [ Run $ Weather "RJTT" [ "--template", "<station> | <skyCondition> | <fc=#4682B4><tempC></fc>°C | <fc=#4682B4><rh></fc>% | <fc=#4682B4><pressure></fc>hPa"
                             ] 36000

        -- Screen brightness
        , Run $ Brightness   [ "--template" , (icon "brightness") ++ " <percent>"
                             , "--"
                             , "-D" , "intel_backlight"
                             ] 10

        -- Volume
        , Run $ Volume "default" "Master"
                             [ "--template" , "<action=pactl set-sink-mute '@DEFAULT_SINK@' toggle><status> <volume>%</action>"
                             , "--"
                             , "--on"       , icon "sound"
                             , "--onc"      , green
                             , "--off"      , icon "sound_mute"
                             , "--offc"     , red
                             ] 10

        -- network activity monitor (dynamic interface resolution)
        , Run $ DynNetwork   [ "--template" , "<dev>: " ++ (afIcon "\xf063") ++ "<rx>kB/s " ++ (afIcon "\xf062") ++ "<tx>kB/s"
                             , "--width"    , "4"
                             , "--Low"      , "1000000"       -- units: B/s
                             , "--High"     , "5000000"       -- units: B/s
                             , "--low"      , green
                             , "--normal"   , yellow
                             , "--high"     , red
                             ] 10

        -- cpu activity monitor
        , Run $ MultiCpu     [ "--template" , (icon "cpu") ++ " <total0>%|<total1>%"
                             , "--width"    , "2"
                             , "--Low"      , "50"         -- units: %
                             , "--High"     , "85"         -- units: %
                             , "--low"      , green
                             , "--normal"   , yellow
                             , "--high"     , red
                             ] 10

        -- cpu core temperature monitor
        , Run $ MultiCoreTemp       [ "--template" , (icon "temperature") ++ "<avg>°C"
                             , "--Low"      , "70"        -- units: °C
                             , "--High"     , "80"        -- units: °C
                             , "--low"      , green
                             , "--normal"   , yellow
                             , "--high"     , red
                             ] 50
                          
        -- memory usage monitor
        , Run $ Memory       [ "--template" , (icon "memory") ++ " <available> M"
                             , "--Low"      , "2000"        -- units: M
                             , "--High"     , "6000"        -- units: M
                             , "--low"      , red
                             , "--normal"   , yellow
                             , "--high"     , green
                             ] 10

        -- battery monitor
        , Run $ Battery        [ "--template" ,  "<acstatus>"

                             , "--" -- battery specific options
                                       -- discharging status
                                       , "-o"    , (icon "battery_bad") ++ "<left>%"
                                       -- AC "on" status
                                       , "-O"    , icon "battery_norm"
                                       -- charged status
                                       , "-i"    , icon "battery"
                             ] 50

        -- time and date indicator 
        --   (%F = y-m-d date, %a = day of week, %T = h:m:s time)
        , Run $ Date           "%F (%a) %T" "date" 10

        -- keyboard layout indicator
        , Run $ Kbd          [ ("ru"         , setColor red "RU")
                             , ("us"         , setColor green "US")
                             ]
        , Run $ DiskU [("/", "/ <usedp>%")]
                    [ "--Low"      , "30"
                    , "--High"     , "60"
                    , "--low"      , green
                    , "--normal"   , yellow
                    , "--high"     , red
                    ]
                    20

        -- Xmonad worspaces and program title
        , Run $ UnsafeStdinReader
        , Run $ Com "bash" ["-c", "~/bin/blocks/pacman_xmobar.sh &"] "updates" 36000
        -- , Run $ Com "bash" ["-c", "~/bin/blocks/music_xmobar.sh &"] "music" 50
        , Run $ PipeReader "/tmp/.music_data" "music"
        ]
   }
main :: IO ()
main = xmobar config
