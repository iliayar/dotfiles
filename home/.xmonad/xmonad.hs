-- Imports
import Data.List
import Data.Monoid

import System.IO (hPutStrLn)
import System.Exit

import Control.Arrow (first)
import Control.Monad (liftM2)

import XMonad

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
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

import qualified XMonad.StackSet as W
import qualified Data.Map        as M


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
myKeys = \conf -> mkKeymap conf $
    [ ("M-."         , sendMessage (IncMasterN (-1))                                                )
    , ("M-,"         , sendMessage (IncMasterN 1)                                                   )
    , ("M-d"         , shellPrompt myXPConfig                                                       )
    , ("M-n"         , refresh                                                                      )
    , ("M-j"         , windows W.focusDown                                                          )
    , ("M-k"         , windows W.focusUp                                                            )
    , ("M-h"         , sendMessage Shrink                                                           )
    , ("M-l"         , sendMessage Expand                                                           )
    , ("M-f"         , withFocused $ windows . W.sink                                               )
    , ("M-b"         , sendMessage ToggleStruts                                                     )
    , ("M-t"         , spawn "killall picom"                                                        )
    , ("M-<F3>"      , spawn "pcmanfm"                                                              )
    , ("M-<Tab>"     , windows W.focusDown                                                          )
    , ("M-<Space>"   , sendMessage NextLayout                                                       )
    , ("M-<Return>"  , spawn $ XMonad.terminal conf                                                 )
    , ("M-S-q"       , kill                                                                         )
    , ("M-S-j"       , windows W.swapDown                                                           )
    , ("M-S-k"       , windows W.swapUp                                                             )
    , ("M-S-c"       , spawn "notify-send 'restarting Xmonad'; xmonad --recompile; xmonad --restart")
    , ("M-S-d"       , spawn "notify-send 'DUNST_COMMAND_TOGGLE'"                                   )
    , ("M-S-<Space>" , windows W.focusMaster                                                        )
    , ("M-S-<Return>", windows W.swapMaster                                                         )
    -- , ("M-C-e"       , io (exitWith ExitSuccess)                                                    )
    , ("M-C-t"       , spawn "/usr/local/bin/picom --experimental-backends -b"                      )
    , ("M-C-x"       , spawn "xkill"                                                                )
    ]
    ++
    (withPrefix "M-S-e"
    [ ("r", spawn "reboot")
    , ("s", spawn "systemctl suspend")
    , ("e", io (exitWith ExitSuccess))
    , ("S-s", spawn "shutdown 0")
    ])
    ++
    [("M" ++ m ++ "-" ++ k, windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) $ map show [1..9] ++ ["m"]
        , (f, m) <- [ (W.greedyView                   , "")
                    , (liftM2 (.) W.greedyView W.shift, "-S")
                    , (W.shift                        , "-C")]]
    -- Multi monitor setup
    -- ++
    -- [("M" ++ m ++ "-" ++ k, screenWorkspace sc >>= flip whenJust (windows . f))
    --     | (k, sc) <- zip ["w", "e", "r"] [0..]
    --     , (f, m) <- [(W.view, ""), (W.shift, "-S")]]
    ++
    [ ("<XF86MonBrightnessUp>"  , spawn "light -A 5"                  )
    , ("<XF86MonBrightnessDown>", spawn "light -U 5"                  )
    , ("<XF86AudioRaiseVolume>" , spawn "pactl set-sink-volume 0 +5%" )
    , ("<XF86AudioLowerVolume>" , spawn "pactl set-sink-volume 0 -5%" )
    , ("<XF86AudioMute>"        , spawn "pactl set-sink-mute 0 toggle")
    ]
    ++
    [ (    "<Print>", scrot "" 0)
    , (  "M-<Print>", scrot "-u" 0)
    , ("M-S-<Print>", scrot "-s" 0.1)
    , ("<XF86AudioNext>", playerctl "next")
    , ("<XF86AudioPrev>", playerctl "previous")
    , ("<XF86AudioPlay>", playerctl "play-pause")
    ]
    where
      scrot p t = spawn $ "sleep " ++ (show t) ++ ";\
                \scrot " ++ p ++ " -e '\
                                \xclip -selection clipboard -t image/png -i $f;\
                                \mv $f ~/Pictures/screenshots/;\
                                \notify-send \"Screenshot saved: $f\";'"
      playerctl a = spawn $ "playerctl " ++ a ++ " -p spotify"
      withPrefix prefix m = [(prefix ++ " " ++ k, a) | (k, a) <- m]

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
          -- spawnOnce "xmobar"
          spawnOnce "stalonetray"
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
        xmproc <- spawnPipe "xmobar -x 0 /home/iliayar/.config/xmobar/xmobarrc"
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
