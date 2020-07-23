-- Imports
import Data.List
import Data.Monoid

import System.IO (hPutStrLn)
import System.Exit

import Control.Arrow (first)

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

import qualified XMonad.StackSet as W
import qualified Data.Map        as M


windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

--------------------------------------------------------
-- Variables
myTerminal      = "termite"

myPlayer        = "spotify"
myBrowser       = "brave"


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
    [ ("M-<Return>"  , spawn $ XMonad.terminal conf)
    , ("M-d"         , shellPrompt myXPConfig)
    , ("M-S-d"       , spawn "gmrun")
    , ("M-S-q"       , kill)
    , ("M-<Space>"   , sendMessage NextLayout)
    , ("M-S-<Space>" , setLayout $ XMonad.layoutHook conf)
    , ("M-n"         , refresh)
    , ("M-<Tab>"     , windows W.focusDown)
    , ("M-j"         , windows W.focusDown)
    , ("M-k"         , windows W.focusUp  )
    , ("M-m"         , windows W.focusMaster  )
    , ("M-S-<Return>", windows W.swapMaster)
    , ("M-S-j"       , windows W.swapDown  )
    , ("M-S-k"       , windows W.swapUp    )
    , ("M-h"         , sendMessage Shrink)
    , ("M-l"         , sendMessage Expand)
    , ("M-t"         , withFocused $ windows . W.sink)
    , ("M-,"         , sendMessage (IncMasterN 1))
    , ("M-."         , sendMessage (IncMasterN (-1)))
    , ("M-b"         , sendMessage ToggleStruts)
    , ("M-C-e"       , io (exitWith ExitSuccess))
    , ("M-S-c"       , spawn "notify-send 'restarting Xmonad'; xmonad --recompile; xmonad --restart")
    ]
    ++
    [("M" ++ m ++ "-" ++ k, windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) $ map show [1..9] ++ ["C-m"]
        , (f, m) <- [(W.greedyView, ""), (W.shift, "-S")]]
    ++
    [("M" ++ m ++ "-" ++ k, screenWorkspace sc >>= flip whenJust (windows . f))
        | (k, sc) <- zip ["w", "e", "r"] [0..]
        , (f, m) <- [(W.view, ""), (W.shift, "-S")]]

myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))
    ]

--------------------------------------------------------
-- Layouts
myLayout = tiled ||| Mirror tiled ||| Full
  where
     tiled   = Tall nmaster delta ratio
     nmaster = 1
     ratio   = 1/2
     delta   = 3/100

--------------------------------------------------------
-- Hooks
myManageHook = composeAll
  [ className =? "Nitrogen"            --> doFloat
  , className =? "stalonetray"         --> doIgnore]

myEventHook = fullscreenEventHook

myStartupHook = do
          spawnOnce "nitrogen --restore &"
          spawnOnce "picom --experimental-backends -b"
          spawnOnce "xbindkeys --poll-rc"
          spawnOnce "/usr/lib/polkit-kde-authentication-agent-1"
          -- spawnOnce "xmobar"
          spawnOnce "stalonetray"
          spawn "xrdb ~/.Xresources"
          setWMName "LG3D"
          --spawnOnce "emacs --daemon &"

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
