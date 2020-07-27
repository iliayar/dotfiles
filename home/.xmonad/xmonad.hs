-- Imports
import Data.List
import Data.Monoid
import Data.Tree

import System.IO (hPutStrLn, hClose, hPutStr, Handle)
import System.Exit

import Control.Arrow (first)
import Control.Monad (liftM2)

import XMonad

import XMonad.Hooks.DynamicLog (xmobarPP, dynamicLogWithPP, xmobarColor, PP(..), wrap, shorten)
import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Hooks.ManageDocks (avoidStruts, manageDocks, docksEventHook, ToggleStruts(..))
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat)
import XMonad.Hooks.SetWMName

import XMonad.Util.SpawnOnce
import XMonad.Util.Run (runProcessWithInput, safeSpawn, spawnPipe)
import XMonad.Util.EZConfig
import XMonad.Util.NamedScratchpad

import XMonad.Prompt
import XMonad.Prompt.Shell (shellPrompt)
import XMonad.Prompt.FuzzyMatch (fuzzyMatch)

import XMonad.Layout.NoBorders
import XMonad.Layout.GridVariants
import XMonad.Layout.Fullscreen (fullscreenEventHook, fullscreenManageHook)

import XMonad.Actions.Submap
import XMonad.Actions.GridSelect

import qualified XMonad.Actions.TreeSelect as TS
import qualified XMonad.StackSet as W
import qualified Data.Map        as M


--------------------------------------------------------
-- Functions

dropRdTuple :: (a, b, c) -> (a, b)
dropRdTuple (a, b, _) = (a, b)
  
getDescription' :: (String, a, String) -> String
getDescription' (k, _, d) = k ++ ": " ++ (takeWhile (/='\n') d)

getDescription :: String -> [(String, b, String)] -> String
getDescription sep keys = intercalate sep $ map getDescription' keys

alignHelp :: String -> String
alignHelp s
  | (length $ lines s) == 1 = s
  | otherwise = intercalate "\n  " $ map alignHelp $ lines s

getHelp' :: (String, a, String) -> String
getHelp' (k, _, d) = "[" ++ k ++ "]: " ++ (alignHelp d)

getHelp :: [(String, a, String)] -> String
getHelp keys = unlines $ map getHelp' keys

escapeSymbols :: String -> String -> String
escapeSymbols esc = concatMap doubleLts
  where
        doubleLts x
          | x `elem` esc = ['\\', x]
          | otherwise = [x]

dzenEscape = escapeSymbols "|"

dzen' :: String -> String
dzen' m = "(echo " ++
  dzenEscape m ++
  " | column; cat) | " ++ unwords [ "dzen2"
                         , "-fn 'Hack Nerd Font Mono 9'"
                         , "-fg '#d5c4a1'"
                         , "-bg '#1d2021'"
                         ]

termShowKeybindings :: String -> X () 
termShowKeybindings m = spawn $ "termite --hold -e \"echo '" ++ m ++  "'\" -t 'termite-keybindings'"

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

xmobarEscape :: String -> String
xmobarEscape = concatMap doubleLts
  where
        doubleLts '<' = "<<"
        doubleLts x   = [x]

addExitMap :: [(String, X (), String)] -> [(String, X (), String)]
addExitMap m = ("<Escape>", io $ return (), "Cancel") : m

dzenKeymapsPipe :: String -> [(String, X (), String)] -> X Handle
dzenKeymapsPipe d m = do
   h <- spawnPipe $ "~/bin/dzenShowKeymap.sh \"" ++ (dzenEscape d) ++ "\""
   io $ hPutStrLn h $ dzenEscape $ getDescription "\n" $ addExitMap m
   io $ hPutStrLn h "END"
   io $ return h

withDzenKeymapsPipe :: String -> [(String, X (), String)] -> X () -> X ()
withDzenKeymapsPipe d m f = do
  h <- dzenKeymapsPipe d m
  f
  io $ hClose h

--------------------------------------------------------
-- Variables

myTerminal      = "termite"

myFont = "xft:Hack Nerd Font Mono:size=9"

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False

myClickJustFocuses :: Bool
myClickJustFocuses = False

myBorderWidth   = 2

myModMask       = mod4Mask

myWorkspaces = map show [1..9] ++ ["Music"]
myWorkspacesClickable    = clickable . (map xmobarEscape) $ (map show [1..9]) ++ ["Music"]
    where
        clickable l = [ "<action=xdotool key super+" ++ n ++ ">" ++ ws ++ "</action>" |
                      (i,ws) <- zip (map show [1..9] ++ ["m"]) l,
                      let n = i ]

myNormalBorderColor  = "#1d2021"
myFocusedBorderColor = "#83a598"

--------------------------------------------------------
-- GridSelect

myColorizer = defaultColorizer

mygridConfig :: p -> GSConfig Window
mygridConfig colorizer = (buildDefaultGSConfig myColorizer)
    { gs_cellheight   = 40
    , gs_cellwidth    = 200
    , gs_cellpadding  = 6
    , gs_originFractX = 0.5
    , gs_originFractY = 0.5
    , gs_font         = myFont
    }

spawnSelected' :: [(String, String)] -> X ()
spawnSelected' lst = gridselect conf lst >>= flip whenJust spawn
    where conf = def
                   { gs_cellheight   = 40
                   , gs_cellwidth    = 200
                   , gs_cellpadding  = 6
                   , gs_originFractX = 0.5
                   , gs_originFractY = 0.5
                   , gs_font         = myFont
                   }

myAppGrid = [ ("Emacs", "emacsclient -c -a emacs")
            , ("Brave", "brave")
            , ("Spotify", "spotify")
            , ("Telegram", "telegram-desktop")
            , ("File Manager", "pcmanfm")
            ]

--------------------------------------------------------
-- ScratchPads

myScratchPads :: [NamedScratchpad]
myScratchPads = [ NS "terminal" spawnTerm findTerm manageTerm
                , NS "notes" spawnNotes findNotes manageNotes
                , NS "weather" spawnWeather findWeather manageWeather
                , NS "ipython" spawnIPython findTerm manageTerm
                ]
  where
    spawnTerm  = myTerminal ++ " -t scratchpad"
    findTerm   = title =? "scratchpad"
    manageTerm = customFloating $ W.RationalRect l t w h
               where
                 h = 0.5
                 w = 1
                 t = 0
                 l = 0
    spawnNotes = "emacsclient -c -a emacs -F '(quote (name . \"emacs-notes\"))' ~/.org/Notes.org"
    findNotes  = title =? "emacs-notes"
    manageNotes = customFloating $ W.RationalRect l t w h
               where
                 h = 0.9
                 w = 0.9
                 t = 0.05
                 l = 0.05
    spawnWeather  = myTerminal ++ " -t weather-scratchpad --hold -e 'bash -c \"curl wttr.in; cat\"'"
    findWeather   = title =? "weather-scratchpad"
    manageWeather = customFloating $ W.RationalRect l t w h
               where
                 h = 0.64
                 w = 0.53
                 t = 0.1
                 l = 0.1
    spawnIPython  = myTerminal ++ " -t scratchpad -e 'ipython'"

--------------------------------------------------------
-- TreeSelect

tsAll =
   [ Node (TS.TSNode "+ Management" "Any management commands" (return ()))
       tsManagement
   , Node (TS.TSNode "+ General" "General purpose applications" (return ()))
       [ Node (TS.TSNode "Pcmanfm" "File Manager" (spawn "pcmanfm")) []
       , Node (TS.TSNode "Brave" "Browser" (spawn "brave")) []
       ]
   , Node (TS.TSNode "+ Programming" "programming" (return ()))
       [ Node (TS.TSNode "IPython" "IPython interactive shell" (namedScratchpadAction myScratchPads "ipython")) []
       , Node (TS.TSNode "Emacs" "IDE/Text editor" (spawn "emacsclient -c -a emacs")) []
       , Node (TS.TSNode "Termite" "Terminal" (spawn "termite")) []
       ]
   ]

tsManagement =
   [ Node (TS.TSNode "Picom" "Picom switch on/off" (return ()))
       [ Node (TS.TSNode "off" "Switch off compositor" (spawn "killall picom")) []
       , Node (TS.TSNode "on" "Switch on compositor" (spawn "/usr/local/bin/picom --experimental-backends -b")) []
       ]
   , Node (TS.TSNode "Brightness" "Set Brightness" (return ()))
       [ Node (TS.TSNode "Max Brightness" "Set Brightness to 100" (spawn "light -S 100")) []
       , Node (TS.TSNode "Norm Brightness" "Set Brightness to 50" (spawn "light -S 50")) []
       , Node (TS.TSNode "Min Brightness" "Set Brightness to 5" (spawn "light -S 5")) []
       ]
   ]

  
tsDefaultConfig :: TS.TSConfig a
tsDefaultConfig = TS.TSConfig { TS.ts_hidechildren = True
                              , TS.ts_background   = 0x00000000
                              , TS.ts_font         = "xft:Hack Nerd Font Mono:size=9"
                              , TS.ts_node         = (0xffd5c4a1, 0xff1d2021)
                              , TS.ts_nodealt      = (0xffd5c4a1, 0xff282828)
                              , TS.ts_highlight    = (0xffffffff, 0xffff4301)
                              , TS.ts_extra        = 0xffd5c4a1
                              , TS.ts_node_width   = 200
                              , TS.ts_node_height  = 25
                              , TS.ts_originX      = 0
                              , TS.ts_originY      = 0
                              , TS.ts_indent       = 80
                              , TS.ts_navigate     = TS.defaultNavigation
                              }
--------------------------------------------------------
-- Keybindings

myKeys = \conf -> let
  createSubmap m = submap $ mkKeymap conf $ map dropRdTuple $ addExitMap m
  prefix p m d = (p, withDzenKeymapsPipe d m $ createSubmap m,
                  d ++ " (submap)\n" ++ getHelp m)
  dzenAllBindings = withDzenKeymapsPipe "Keybindings" keymap $ createSubmap []
  restartRecompile = intercalate " && "
    [ "cd ~/.xmonad"
    , "stack ghc -- --make ~/.config/xmobar/xmobar.hs"
    , "xmonad --recompile"
    , "xmonad --restart"
    , "notify-send 'restarting Xmonad'"
    ]
  keymap =
    [ ("M-."         , sendMessage (IncMasterN (-1))                          , "Decrease Master N"        )
    , ("M-,"         , sendMessage (IncMasterN 1)                             , "Increase Master N"        )
    , ("M-b"         , sendMessage ToggleStruts                               , "Hide bar"                 )
    , ("M-d"         , shellPrompt myXPConfig                                 , "Prompt"                   )
    , ("M-f"         , withFocused $ windows . W.sink                         , "Make window tiled"        )
    , ("M-h"         , sendMessage Shrink                                     , "Shrink window"            )
    , ("M-j"         , windows W.focusDown                                    , "Prev window"              )
    , ("M-k"         , windows W.focusUp                                      , "Next window"              )
    , ("M-l"         , sendMessage Expand                                     , "Expand window"            )
    , ("M-<Space>"   , sendMessage NextLayout                                 , "Cicle layouts"            )
    , ("M-<Return>"  , spawn $ XMonad.terminal conf                           , "Launch terminal"          )
    , ("M-S-/"       , termShowKeybindings $ getHelp keymap                   , "Show this help"           )
    , ("M-S-c"       , spawn restartRecompile                                 , "Recompile, restart XMonad")
    , ("M-S-d"       , spawn "notify-send 'DUNST_COMMAND_TOGGLE'"             , "Toggle notifications"     )
    , ("M-S-j"       , windows W.swapDown                                     , "Swap window with prev"    )
    , ("M-S-k"       , windows W.swapUp                                       , "Swap window with next"    )
    , ("M-S-q"       , kill                                                   , "Close window"             )
    , ("M-S-<Space>" , windows W.focusMaster                                  , "Focus master window"      )
    , ("M-S-<Return>", windows W.swapMaster                                   , "Make window master"       )
    , ("M-C-/"       , dzenAllBindings                                        , "Show this help"           )
    , ("M-C-x"       , spawn "xkill"                                          , "Launch xkill"             )
    , prefix "M-C-<Return>"
      [ ("t", namedScratchpadAction myScratchPads "terminal", "Terminal scratchpad" )
      , ("n", namedScratchpadAction myScratchPads "notes"   , "Notes.org scratchpad")
      , ("w", namedScratchpadAction myScratchPads "weather" , "Weather scratchpad")
      , ("p", namedScratchpadAction myScratchPads "ipython" , "IPython intercative shell")
      ] "Sratchpads"
    , prefix "M-S-e"
       [ ("r"  , spawn "reboot"           , "Reboot"     )
       , ("s"  , spawn "systemctl suspend", "Suspend"    )
       , ("e"  , io (exitWith ExitSuccess), "Exit XMonad")
       , ("S-s", spawn "shutdown 0"       , "Shutdown"   )
       ] "Power management"
    , prefix "M-g"
      [ ("g", spawnSelected' myAppGrid                , "Grid select favorite apps")
      , ("t", goToSelected $ mygridConfig myColorizer , "Goto selected window"     )
      , ("b", bringSelected $ mygridConfig myColorizer, "Bring selected window"    )
      ] "GridSelect"
    , prefix "M-t"
      [ ("a", TS.treeselectAction tsDefaultConfig tsAll, "TreeSelect All")
      , ("m", TS.treeselectAction tsDefaultConfig tsManagement, "TreeSelect Management")
      ] "TreesSlect"
    ]
    ++
    [("M" ++ m ++ "-" ++ k, windows $ f i
    , (case m of
         ""   -> "Switch"
         "-S" -> "Move window and switch"
         "-C" -> "Move window")
       ++ " to workspace " ++ n)
        | (n, i, k) <- zip3 myWorkspaces (XMonad.workspaces conf) $ map show [1..9] ++ ["m"]
        , (f, m) <- [ (W.greedyView                   , "")
                    , (liftM2 (.) W.greedyView W.shift, "-S")
                    , (W.shift                        , "-C")]]
    ++
    [("M" ++ m ++ "-" ++ k, screenWorkspace sc >>= flip whenJust (windows . f)
    , (case m of
         ""   -> "Switch"
         "-S" -> "Move window")
      ++ " to screen " ++ (show sc))
        | (k, sc) <- zip ["p", "[", "]"] [0..]
        , (f, m) <- [(W.view, ""), (W.shift, "-S")]]
    ++
    [ ("<XF86MonBrightnessUp>"  , spawn "light -A 5"                  , "Brightness up"  )
    , ("<XF86MonBrightnessDown>", spawn "light -U 5"                  , "Brightness down")
    , ("<XF86AudioRaiseVolume>" , spawn "pactl set-sink-volume 0 +5%" , "Audio up"       )
    , ("<XF86AudioLowerVolume>" , spawn "pactl set-sink-volume 0 -5%" , "Audio down"     )
    , ("<XF86AudioMute>"        , spawn "pactl set-sink-mute 0 toggle", "Mute/Unmute"    )
    ]
    ++
    [ (    "<Print>", scrot "" 0, "Screenshot"                      )
    , (  "M-<Print>", scrot "-u" 0, "Screenshot of active window"   )
    , ("M-S-<Print>", scrot "-s" 0.1, "Screenshot interactive"      )
    , ("<XF86AudioNext>", playerctl "next", "Music next"            )
    , ("<XF86AudioPrev>", playerctl "previous", "Music prev"        )
    , ("<XF86AudioPlay>", playerctl "play-pause", "Music play/pause")
    ]
  in mkKeymap conf $ map dropRdTuple keymap
    where
      scrot p t = spawn $ "sleep " ++ (show t) ++ ";\
                \scrot " ++ p ++ " -e '\
                                \xclip -selection clipboard -t image/png -i $f;\
                                \mv $f ~/Pictures/screenshots/;\
                                \notify-send \"Screenshot saved: $f\";'"
      playerctl a = spawn $ "playerctl " ++ a ++ " -p spotify"

myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))
    ]

--------------------------------------------------------
-- Layouts
myLayout =  tiled ||| Mirror tiled ||| Grid (16/10) ||| Full
  where
     tiled   = Tall nmaster delta ratio
     nmaster = 1
     ratio   = 1/2
     delta   = 3/100

--------------------------------------------------------
-- Hooks
myManageHook = composeAll
  [ className =? "Nitrogen"            --> doFloat
  , className =? "feh"                 --> doFloat
  , resource  =? "stalonetray"         --> doIgnore
  , title     =? "termite-keybindings" --> (customFloating $ W.RationalRect 0 0 0.5 1)
  ] <+> namedScratchpadManageHook myScratchPads

myEventHook = fullscreenEventHook

myStartupHook = do
          spawnOnce "nitrogen --restore &"
          spawnOnce "picom --experimental-backends -b"
          spawnOnce "/usr/lib/polkit-kde-authentication-agent-1"
          spawnOnce "stalonetray"
          spawnOnce "xsetroot -cursor_name arrow"
          spawn "xrdb ~/.Xresources"
          setWMName "LG3D"
          spawnOnce "emacs --daemon &"

--------------------------------------------------------
-- Prompt
myXPConfig :: XPConfig
myXPConfig = def
      { font                = myFont
      , bgColor             = "#1d2021"
      , fgColor             = "#eddbb2"
      , bgHLight            = "#282828"
      , fgHLight            = "#fb4934"
      , borderColor         = "#83a598"
      , promptBorderWidth   = 0
      , promptKeymap        = defaultXPKeymap
      , position            = Top
--    , position            = CenteredAt { xpCenterY = 0.3, xpWidth = 0.3 }
      , height              = 20
      , historySize         = 256
      , historyFilter       = id
      , defaultText         = []
      , autoComplete        = Nothing  -- set Just 100000 for .1 sec
      , showCompletionOnTab = False
      , searchPredicate     = fuzzyMatch
      , alwaysHighlight     = True
      , maxComplRows        = Nothing      -- set to Just 5 for 5 rows
      }

--------------------------------------------------------
-- Main
main = do
        xmproc <- spawnPipe "xmobar -x 0 /home/iliayar/.config/xmobar/xmobar.hs"
        xmonad $ ewmh def {
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        clickJustFocuses   = myClickJustFocuses,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspacesClickable,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,
        keys               = myKeys,
        mouseBindings      = myMouseBindings,
        layoutHook         = avoidStruts $ smartBorders $ myLayout,
        manageHook         = (isFullscreen --> doFullFloat) <+> myManageHook <+> manageDocks,
        handleEventHook    = myEventHook <+> docksEventHook,
        logHook            = dynamicLogWithPP xmobarPP
                { ppOutput  = \x -> hPutStrLn xmproc x
                , ppCurrent = xmobarColor "#b8bb26" "" . wrap "[" "]"
                , ppVisible = xmobarColor "#b8bb26" ""
                , ppTitle   = xmobarColor "#fb4934" "" . shorten 30
                , ppExtras  = []-- [windowCount]                           -- # of windows current workspace
                , ppOrder   = \(ws:l:t:ex) -> [ws,l]++ex++[t] -- workspaces : layout : extras : title
                },
        startupHook        = myStartupHook
    }
