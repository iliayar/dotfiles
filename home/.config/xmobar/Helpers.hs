module Helpers where

icon :: String -> String
icon f = "<icon=" ++ f ++".xpm/>"

afIcon :: String -> String
afIcon s = "<fn=1>" ++ s ++ "</fn>"

emoji :: String -> String
emoji s = "<fn=2>" ++ s ++ "</fn>"

setColor :: String -> String -> String
setColor color s = "<fc=" ++ color ++ ">" ++ s ++ "</fc>"

font' = "xft:Fira Code:size=9:bold:antialias=true"
additionalFonts' = [ "xft:FontAwesome5Brands:pixelsize=14:antialias=true,FontAwesome5Free:style=Solid:pixelsize=14:antialias=true,Noto Color Emoji:pixelsize=14:antialias=true" ]
-- additionalFonts' = ["xft:FontAwesome5Brands:pixelsize=14:antialias=true,FontAwesome5Free:style=Solid:pixelsize=14,all-the-icons:size=50:antialias=true"]
