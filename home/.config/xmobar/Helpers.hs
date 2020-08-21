module Helpers where

data Theme = Theme { red :: String
                   , green :: String
                   , yellow :: String
                   , fg :: String
                   , bg :: String
                   }


fetchTheme :: IO Theme
fetchTheme = return $ Theme { red    = "#fb4934"
                            , green  = "#fabd2f"
                            , yellow = "#b8bb26"
                            , fg     = "#ebdbb2"
                            , bg     = "#1d2021"
                            }

icon :: String -> String
icon f = "<icon=" ++ f ++".xpm/>"

afIcon :: String -> String
afIcon s = "<fn=1>" ++ s ++ "</fn>"

setColor :: String -> String -> String
setColor color s = "<fc=" ++ color ++ ">" ++ s ++ "</fc>"

font' = "xft:Hack:size=8:bold:antialias=true"
additionalFonts' = ["xft:FontAwesome5Brands:pixelsize=14,FontAwesome5Free:style=Solid:pixelsize=14"]
