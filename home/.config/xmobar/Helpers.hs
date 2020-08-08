module Helpers where

red, green, yellow :: String
red    = "#fb4934"
green  = "#b8bb26"
yellow = "#fabd2f"

icon :: String -> String
icon f = "<icon=" ++ f ++".xpm/>"

afIcon :: String -> String
afIcon s = "<fn=1>" ++ s ++ "</fn>"

setColor :: String -> String -> String
setColor color s = "<fc=" ++ color ++ ">" ++ s ++ "</fc>"
