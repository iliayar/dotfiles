module Helpers where

icon :: String -> String
icon f = "<icon=" ++ f ++".xpm/>"

afIcon :: String -> String
afIcon s = "<fn=1>" ++ s ++ "</fn>"

emoji :: String -> String
emoji s = "<fn=2>" ++ s ++ "</fn>"

setColor :: String -> String -> String
setColor color s = "<fc=" ++ color ++ ">" ++ s ++ "</fc>"

font' = "Fira Code Bold 8"
additionalFonts' = ["Font Awesome 6 Brands Solid 10, Font Awesome 6 Free Solid 10", "Noto Color Emoji 10"]
