import Xmobar

-- TODO xresources
red, green, yellow :: String
red    = "#fb4934"
green  = "#b8bb26"
yellow = "#fabd2f"

icon :: String -> String
icon f = "<icon=" ++ f ++".xpm/>"

awIcon :: String -> String
awIcon s = "<fn=1>" ++ s ++ "</fn>"

setColor :: String -> String -> String
setColor color s = "<fc=" ++ color ++ ">" ++ s ++ "</fc>"

  
config :: Config
config = defaultConfig { 

   -- appearance
     font =         "xft:Hack-8:bold:antialias=true"
   , bgColor     =  "#1d2021"
   , additionalFonts = ["xft:FontAwesome-8"]
   , fgColor     =  "#ebdbb2"
   , alpha       =  255
   , position   = OnScreen 0 $ Bottom
--    , position    =  Static {xpos = 0, ypos = 1060, width = 1364, height = 20}
   , borderColor =  "#646464"

   -- layout
   , sepChar =  "%"   -- delineator between plugin names and straight text
   , alignSep = "}{"  -- separator between left-right alignment
   , template = "<action=~/.xmonad/xmonadctl 1>" ++ (icon "haskell_20") ++ "</action> | %UnsafeStdinReader% }{ %music% | %updates% | %disku% | %bright% | %default:Master% | %kbd% | %memory% | %multicpu% | %dynnetwork% | %battery% | %multicoretemp% | %date% "

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
        , Run $ DynNetwork   [ "--template" , "<dev>: " ++ (awIcon "\xf063") ++ "<rx>kB/s " ++ (awIcon "\xf062") ++ "<tx>kB/s"
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
        , Run $ Memory       [ "--template" , (icon "memory") ++ "<fn=1>\xf538</fn> <available> M"
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
