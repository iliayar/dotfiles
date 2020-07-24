-- Imports
import Data.List
import Data.Monoid

import System.IO (hPutStrLn)
import System.Exit

import Control.Arrow (first)
import Control.Monad (liftM2)

import XMonad

import XMonad.Hooks.DynamicLog (ppOutput, ppExtras, ppOrder, xmobarPP, dynamicLogWithPP)
import XMonad.Hooks.EwmhDesktops (fullscreenEventHook, ewmh)
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName

import XMonad.Util.SpawnOnce
import XMonad.Util.Run (runProcessWithInput, safeSpawn, spawnPipe)
import XMonad.Util.EZConfig

import XMonad.Prompt
import XMonad.Prompt.Shell (shellPrompt)
import XMonad.Prompt.FuzzyMatch (fuzzyMatch)

import XMonad.Layout.NoBorders
import XMonad.Layout.GridVariants

import XMonad.Actions.Submap

import qualified XMonad.StackSet as W
import qualified Data.Map        as M


--------------------------------------------------------
-- Functions

dropRdTuple (a, b, _) = (a, b)
describeSubmap c (a, b) = (a, b, c ++ " (submap)")
  
getDescription' (k, _, d) = "[" ++ k ++ "]: " ++ d
getDescription keys = intercalate " | " $ map getDescription' keys

escapeSymbols = "<>()|"
dzen m = spawn $ "echo " ++
  (foldl (\acc e -> acc ++ (if elem e escapeSymbols then ['\\', e] else [e])) "" m) ++
  " | dzen2 -p 5 \
  \-fn 'Hack Nerd Font Mono 9' \
  \-fg '#d5c4a1' \
  \-bg '#1d2021'"
  
xmessage m = spawn $ "xmessage '" ++ m ++ "'" 

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

--------------------------------------------------------
-- Variables
myTerminal      = "termite"

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False

myClickJustFocuses :: Bool
myClickJustFocuses = False

myBorderWidth   = 2

myModMask       = mod4Mask

myWorkspaces    = map show [1..9] ++ ["Music"] 

myNormalBorderColor  = "#1d2021"
myFocusedBorderColor = "#83a598"

--------------------------------------------------------
-- Keybindings
myKeys = \conf -> let
  addExitMap m = ("<Escape>", dzen "Exited submap", "Cancel") : m
  prefix p m = (p, (dzen $ getDescription $ addExitMap m) >>
                 (submap $ mkKeymap conf $ map dropRdTuple $ addExitMap m))
  keymap =
    [ ("M-."         , sendMessage (IncMasterN (-1))                          , "Decrease Master N"        )
    , ("M-,"         , sendMessage (IncMasterN 1)                             , "Increase Master N"        )
    , ("M-b"         , sendMessage ToggleStruts                               , "Hide bar"                 )
    , ("M-d"         , shellPrompt myXPConfig                                 , "Prompt"                   )
    , ("M-f"         , withFocused $ windows . W.sink                         , "Make window tiled"        )
    , ("M-g"         , spawn "brave"                                          , "Launch Brave"             )
    , ("M-h"         , sendMessage Shrink                                     , "Shrink window"            )
    , ("M-j"         , windows W.focusDown                                    , "Prev window"              )
    , ("M-k"         , windows W.focusUp                                      , "Next window"              )
    , ("M-l"         , sendMessage Expand                                     , "Expand window"            )
    , ("M-t"         , spawn "killall picom"                                  , "Disable picom"            )
    , ("M-<F3>"      , spawn "pcmanfm"                                        , "File browser"             )
    , ("M-<Space>"   , sendMessage NextLayout                                 , "Cicle layouts"            )
    , ("M-<Return>"  , spawn $ XMonad.terminal conf                           , "Launch terminal"          )
    , ("M-S-/"       , xmessage $ unlines $ map getDescription' keymap        , "Show this help"           )
    , ("M-S-c"       , spawn "cd ~/.xmonad &&  \
        \stack ghc -- --make ~/.config/xmobar/xmobar.hs && \
        \xmonad --recompile && \
        \xmonad --restart && \
        \notify-send 'restarting Xmonad'"                                     , "Recompile, restart XMonad")
    , ("M-S-d"       , spawn "notify-send 'DUNST_COMMAND_TOGGLE'"             , "Toggle notifications"     )
    , ("M-S-j"       , windows W.swapDown                                     , "Swap window with prev"    )
    , ("M-S-k"       , windows W.swapUp                                       , "Swap window with next"    )
    , ("M-S-q"       , kill                                                   , "Close window"             )
    , ("M-S-<Space>" , windows W.focusMaster                                  , "Focus master window"      )
    , ("M-S-<Return>", windows W.swapMaster                                   , "Make window master"       )
    , ("M-C-t"       , spawn "/usr/local/bin/picom --experimental-backends -b", "Start picom"              )
    , ("M-C-x"       , spawn "xkill"                                          , "Launch xkill"             )
    , describeSubmap "Power management" $ prefix "M-S-e"
       [ ("r"  , spawn "reboot"           , "Reboot")
       , ("s"  , spawn "systemctl suspend", "Suspend")
       , ("e"  , io (exitWith ExitSuccess), "Exit XMonad")
       , ("S-s", spawn "shutdown 0"       , "Shutdown")
       ]
    ]
    ++
    [("M" ++ m ++ "-" ++ k, windows $ f i
    , (case m of
         ""   -> "Switch"
         "-S" -> "Move window and switch"
         "-C" -> "Move window")
       ++ " to workspace " ++ i)
        | (i, k) <- zip (XMonad.workspaces conf) $ map show [1..9] ++ ["m"]
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
    [ ("<XF86MonBrightnessUp>"  , spawn "light -A 5"                  , "Brightness up")
    , ("<XF86MonBrightnessDown>", spawn "light -U 5"                  , "Brightness down")
    , ("<XF86AudioRaiseVolume>" , spawn "pactl set-sink-volume 0 +5%" , "Audio up")
    , ("<XF86AudioLowerVolume>" , spawn "pactl set-sink-volume 0 -5%" , "Audio down")
    , ("<XF86AudioMute>"        , spawn "pactl set-sink-mute 0 toggle", "Mute/Unmute")
    ]
    ++
    [ (    "<Print>", scrot "" 0, "Screenshot")
    , (  "M-<Print>", scrot "-u" 0, "Screenshot of active window")
    , ("M-S-<Print>", scrot "-s" 0.1, "Screenshot interactive")
    , ("<XF86AudioNext>", playerctl "next", "Music next")
    , ("<XF86AudioPrev>", playerctl "previous", "Music prev")
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
  ]

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
      { font                = "xft:Hack Nerd Font Mono:size=9"
      , bgColor             = "#1d2021"
      , fgColor             = "#d5c4a1"
      , bgHLight            = "#665c54"
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
        xmonad $ docks $ ewmh def {
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        clickJustFocuses   = myClickJustFocuses,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,
        keys               = myKeys,
        mouseBindings      = myMouseBindings,
        layoutHook         = avoidStruts $ smartBorders $ myLayout,
        manageHook         = myManageHook,
        handleEventHook    = myEventHook,
        logHook            = dynamicLogWithPP xmobarPP
                { ppOutput = \x -> hPutStrLn xmproc x
                , ppExtras  = []-- [windowCount]                           -- # of windows current workspace
                , ppOrder  = \(ws:l:t:ex) -> [ws,l]++ex++[t] -- workspaces : layout : extras : title
                },
        startupHook        = myStartupHook
    }
