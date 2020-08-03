import Xmobar

-- TODO xresources
red, green, yellow :: String
red    = "#cc241d"
green  = "#b8bb26"
yellow = "#fb4934"

setColor :: String -> String -> String
setColor color s = "<fc=" ++ color ++ ">" ++ s ++ "</fc>"

  
config :: Config
config = defaultConfig { 

   -- appearance
     font =         "xft:Hack Nerd Font Mono:size=8:bold:antialias=true"
   , bgColor     =  "#1d2021"
   , fgColor     =  "#eddbb2"
   , alpha       =  255
   , position   = OnScreen 0 $ BottomP 0 99
--    , position    =  Static {xpos = 0, ypos = 1060, width = 1364, height = 20}
   , borderColor =  "#646464"

   -- layout
   , sepChar =  "%"   -- delineator between plugin names and straight text
   , alignSep = "}{"  -- separator between left-right alignment
   , template = "<action=~/.xmonad/xmonadctl 1><icon=haskell_20.xpm/></action> | %UnsafeStdinReader% }{ %music% | %updates% | %disku% | %bright% | %default:Master% | %kbd% | %memory% | %multicpu% | %dynnetwork% | %battery% | %multicoretemp% | %date% "

   -- general behavior
   , lowerOnStart =     True    -- send to bottom of window stack on start
   , hideOnStart =      False   -- start with window unmapped (hidden)
   , allDesktops =      True    -- show on all desktops
   , overrideRedirect = True    -- set the Override Redirect flag (Xlib)
   , pickBroadest =     False   -- choose widest display (multi-monitor)
   , persistent =       True    -- enable/disable hiding (True = disabled)
   , iconRoot = "/home/iliayar/.xmonad/xpm/"  -- default: "."

   -- plugins
   --   Numbers can be automatically colored according to their value. xmobar
   --   decides color based on a three-tier/two-cutoff system, controlled by
   --   command options:
   --     --Low sets the low cutoff
   --     --High sets the high cutoff
   --
   --     --low sets the color below --Low cutoff
   --     --normal sets the color between --Low and --High cutoffs
   --     --High sets the color above --High cutoff
   --
   --   The --template option controls how the plugin is displayed. Text
   --   color can be set by enclosing in <fc></fc> tags. For more details
   --   see http://projects.haskell.org/xmobar/#system-monitor-plugins.
   , commands = 

        -- weather monitor
        -- %RJTT% in template
        -- RJTT is station id
        [ Run $ Weather "RJTT" [ "--template", "<station> | <skyCondition> | <fc=#4682B4><tempC></fc>°C | <fc=#4682B4><rh></fc>% | <fc=#4682B4><pressure></fc>hPa"
                             ] 36000

        -- Screen brightness
        , Run $ Brightness   [ "--template" , "\62943 <percent>"
                             , "--"
                             , "-D" , "intel_backlight"
                             ] 10

        -- Volume
        , Run $ Volume "default" "Master"
                             [ "--template" , "<action=pactl set-sink-mute '@DEFAULT_SINK@' toggle><status> <volume>%</action>"
                             , "--"
                             , "--on"       , "\61602"
                             , "--onc"      , green
                             , "--off"      , "\61943"
                             , "--offc"     , red
                             ] 10

        -- network activity monitor (dynamic interface resolution)
        , Run $ DynNetwork   [ "--template" , "<dev>: \61813<rx>kB/s \61814<tx>kB/s"
                             , "--width"    , "4"
                             , "--Low"      , "1000"       -- units: B/s
                             , "--High"     , "5000"       -- units: B/s
                             , "--low"      , green
                             , "--normal"   , yellow
                             , "--high"     , red
                             ] 10

        -- cpu activity monitor
        , Run $ MultiCpu     [ "--template" , "\63578 <total0>%|<total1>%"
                             , "--width"    , "2"
                             , "--Low"      , "50"         -- units: %
                             , "--High"     , "85"         -- units: %
                             , "--low"      , green
                             , "--normal"   , yellow
                             , "--high"     , red
                             ] 10

        -- cpu core temperature monitor
        , Run $ MultiCoreTemp       [ "--template" , "\57866<avg>°C"
                             , "--Low"      , "70"        -- units: °C
                             , "--High"     , "80"        -- units: °C
                             , "--low"      , green
                             , "--normal"   , yellow
                             , "--high"     , red
                             ] 50
                          
        -- memory usage monitor
        , Run $ Memory       [ "--template" ,"\62611 [<used>M] <usedratio>%"
                             , "--Low"      , "20"        -- units: %
                             , "--High"     , "90"        -- units: %
                             , "--low"      , green
                             , "--normal"   , yellow
                             , "--high"     , red
                             ] 10

        -- battery monitor
        , Run $ Battery        [ "--template" , "\62016 <acstatus>"
                             , "--Low"      , "10"        -- units: %
                             , "--High"     , "80"        -- units: %
                             , "--low"      , red
                             , "--normal"   , yellow
                             , "--high"     , green

                             , "--" -- battery specific options
                                       -- discharging status
                                       , "-o"    , "<left>% (<timeleft>)"
                                       -- AC "on" status
                                       , "-O"    , setColor yellow "Charging"
                                       -- charged status
                                       , "-i"    , setColor green "Charged"
                             ] 50

        -- time and date indicator 
        --   (%F = y-m-d date, %a = day of week, %T = h:m:s time)
        , Run $ Date           "%F (%a) %T" "date" 10

        -- keyboard layout indicator
        , Run $ Kbd          [ ("ru"         , setColor red "RU")
                             , ("us"         , setColor green "US")
                             ]
        , Run $ DiskU [("/", "/ <usedp>%")]
                    [ "--Low"      , "20"
                    , "--High"     , "50"
                    , "--low"      , green
                    , "--normal"   , yellow
                    , "--high"     , red
                    ]
                    20

        -- Xmonad worspaces and program title
        , Run $ UnsafeStdinReader
        , Run $ Com "bash" ["-c", "~/bin/blocks/pacman_xmobar.sh &"] "updates" 36000
        , Run $ Com "bash" ["-c", "~/bin/blocks/music_xmobar.sh"] "music" 10
        ]
   }
main :: IO ()
main = xmobar config
